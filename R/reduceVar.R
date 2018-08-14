#' Reduce Variables
#'
#' Remove variables whose contribution (permutation or percent) is less than the
#' given threshold. The function removes one variable at time and after trains a
#' new model to get the new variable contribution rank. If use_jk is TRUE the
#' function checks if after removing the variable the model performance decreases
#' (according to the given metric). In this case the function stops removing the
#' variable even if the contribution is lower than the given threshold.
#'
#' @param model Maxent object.
#' @param th numeric. The contribtion threshold used to remove variables, default is 2.
#' @param metric character. The metric used to evaluate the models, possible values are:
#' "auc", "tss" and "aicc", used only if use_jk is TRUE, default is "auc".
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#' Used only with AICc.
#' @param use_percent logical, if TRUE it uses the percent contribution instead of the
#' permutation importance.
#' @param use_jk Flag to use the Jackknife AUC test during the variable selection,
#' if FALSE the function uses the percent variable contribution, default is FALSE.
#'
#' @return The model trained using the selected variables.
#' @export
#'
#' @examples
#' \dontrun{model <- reduveVar(model = model, th = 2)}
#'
#' @author Sergio Vignali
reduceVar <- function(model, th = 2, metric = c("auc", "tss", "aicc"),
                      env = NULL, parallel = FALSE, use_percent = FALSE,
                      use_jk = FALSE) {

  metric <- match.arg(metric)

  if (nrow(model@test@data) == 0 & metric != "aicc")
    stop(paste("Model must have a test dataset if you want to use", metric,
               "metric!"))

  variables_reduced = FALSE

  while (variables_reduced == FALSE) {

    scores <- varImp(model)
    if (use_percent) {
      scores <- scores[order(scores$Percent_contribution), ]
      i <- 3
    } else {
      scores <- scores[order(scores$Permutation_importance), ]
      i <- 2
    }

    if (scores[1, i] < th) {
      jk_test <- suppressMessages(doJk(model, variables = scores[1, 1],
                                       metric = metric, with_only = FALSE,
                                       return_models = TRUE, env = env,
                                       parallel = parallel))
      if (use_jk) {
        if (metric == "auc") {
          old_metric <- model@results["Test.AUC", ]
          new_metric <- jk_test$results[1, 3]
          if (new_metric >= old_metric) {
            model <- jk_test$models_without[[1]]
          } else {
            variables_reduced <- TRUE
          }
        } else if (metric == "tss") {
          old_metric <- tss(model)
          new_metric <- jk_test$results[1, 3]
          if (new_metric >= old_metric) {
            model <- jk_test$models_without[[1]]
          } else {
            variables_reduced <- TRUE
          }
        } else {
          old_metric <- aicc(model, env = env, parallel = parallel)
          new_metric <- jk_test$results[1, 2]
          if (new_metric <= old_metric) {
            model <- jk_test$models_without[[1]]
          } else {
            variables_reduced <- TRUE
          }
        }
      } else {
        model <- jk_test$models_without[[1]]
      }
    } else {
      variables_reduced <- TRUE
    }
  }

  gc()

  return(model)
}
