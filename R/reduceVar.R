#' Reduce Variables
#'
#' Remove variables whose contribution (permutation or percent) is less than the
#' given threshold. The function removes one variable at time and after trains a
#' new model to get the new variable contribution rank. If use_jk is TRUE the
#' function checks if after removing the variable the model performance
#' decreases (according to the given metric). In this case the function stops
#' removing the variable even if the contribution is lower than the given
#' threshold.
#'
#' @param model SDMmodel object.
#' @param th numeric. The contribution threshold used to remove variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", used only if use_jk is TRUE, default is
#' "auc".
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc,
#'  and if use_jk = FALSE, default is NULL.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param use_jk Flag to use the Jackknife AUC test during the variable
#' selection, if FALSE the function uses the percent variable contribution,
#' default is FALSE.
#' @param permut integer. Number of permutations, default is 10.
#'
#' @return The model trained using the selected variables.
#' @export
#'
#' @examples
#' \dontrun{model <- reduveVar(model = model, th = 2)}
#'
#' @author Sergio Vignali
reduceVar <- function(model, th, metric = c("auc", "tss", "aicc"),
                      test = NULL, env = NULL, parallel = FALSE,
                      use_jk = FALSE, permut = 10) {

  metric <- match.arg(metric)

  if (use_jk == TRUE & is.null(test) & metric != "aicc")
    stop("You need to provide a test dataset!")
  if (use_jk == TRUE & metric == "aicc" & is.null(env))
    stop("You must provide the env argument if you want to use AICc metric!")

  variables_reduced = FALSE

  while (variables_reduced == FALSE) {

    scores <- varImp(model, permut = permut)
    scores <- scores[order(scores$Permutation_importance), ]

    if (scores[1, 2] < th) {
      jk_test <- suppressMessages(doJk(model,
                                       variables = as.character(scores[1, 1]),
                                       metric = metric, test = test,
                                       with_only = FALSE,
                                       return_models = TRUE, env = env,
                                       parallel = parallel))
      if (use_jk) {
        if (metric == "auc") {
          old_metric <- auc(model)
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
