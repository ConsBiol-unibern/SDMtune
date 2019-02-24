#' Reduce Variables
#'
#' Remove variables whose permutation importance is less than the
#' given threshold. The function removes one variable at time and after trains a
#' new model to get the new variable contribution rank. If use_jk is TRUE the
#' function checks if after removing the variable the model performance
#' decreases (according to the given metric). In this case the function stops
#' removing the variable even if the contribution is lower than the given
#' threshold.
#'
#' @param model S\link{DMmodel} or \link{SDMmodelCV} object.
#' @param th numeric. The contribution threshold used to remove variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", used only if use_jk is TRUE, default is
#' "auc".
#' @param test \link{SWD}. Test dataset used to evaluate the model, not used
#' with aicc, and if use_jk = FALSE, default is NULL.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
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

  variables_reduced <- FALSE

  # metric used for chart
  train_metric <- data.frame(x = 0, y = .get_metric(metric, model, env = env,
                                                    parallel = parallel))
  if (metric != "aicc") {
    val_metric <- data.frame(x = 0, y = .get_metric(metric, model, test = test))
  } else {
    val_metric <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Create chart
  initial_vars <- colnames(model@p@data)
  settings <- list(labels = initial_vars, metric = .get_metric_label(metric),
                   update = TRUE)
  data = list(data = rep(0, length(initial_vars)), stop = FALSE)
  folder <- tempfile("SDMsel")

  .create_chart(folder = folder, script = "varSelection.js",
                settings = settings, data = data, height = 600)
  Sys.sleep(1.5)

  while (variables_reduced == FALSE) {

    continue_jk <- FALSE
    scores <- suppressMessages(varImp(model, permut = permut))

    # Update chart
    index <- match(initial_vars, scores[, 1])
    vals <- scores[, 2][index]
    vals[is.na(vals)] <- 0
    data = list(data = vals, train = train_metric, val = val_metric,
                stop = FALSE)
    .update_chart(folder, data = data)
    Sys.sleep(.1)

    scores <- scores[order(scores$Permutation_importance), ]
    scores <- scores[scores$Permutation_importance <= th, ]

    # index for metric data frames
    x <- nrow(train_metric) + 1

    if (nrow(scores) > 0) {
      if (use_jk) {
        for (i in 1:nrow(scores)) {
          jk_test <- suppressMessages(doJk(model,
                                           variables = as.character(scores[i, 1]),
                                           metric = metric, test = test,
                                           with_only = FALSE,
                                           return_models = TRUE, env = env,
                                           parallel = parallel))

          new_metric <- jk_test$results[1, 2]

          if (metric  != "aicc") {
            if (new_metric >= train_metric[x - 1, 2]) {
              model <- jk_test$models_without[[1]]
              train_metric[x, ] <- list(x = x - 1, y = new_metric)
              val_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 3])
              continue_jk <- TRUE
              break
            }
          } else {
            if (new_metric <= train_metric[x - 1, 2]) {
              model <- jk_test$models_without[[1]]
              train_metric[x, ] <- list(x = x - 1, y = new_metric)
              continue_jk <- TRUE
              break
            }
          }
        }
        if (continue_jk) {
          next
        } else {
          scores <- suppressMessages(varImp(model, permut = permut))

          # Update chart
          index <- match(initial_vars, scores[, 1])
          vals <- scores[, 2][index]
          vals[is.na(vals)] <- 0
          data = list(data = vals, train = train_metric, val = val_metric,
                      stop = FALSE)
          variables_reduced <- TRUE
        }
      } else {
        jk_test <- suppressMessages(doJk(model,
                                         variables = as.character(scores[1, 1]),
                                         metric = metric, test = test,
                                         with_only = FALSE,
                                         return_models = TRUE, env = env,
                                         parallel = parallel))
        model <- jk_test$models_without[[1]]
        train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])
        if (metric != "aicc")
          val_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 3])
      }
    } else {
      variables_reduced <- TRUE
    }
  }

  .update_chart(folder, data = list(data = vals, train = train_metric,
                                    val = val_metric, stop = TRUE))
  Sys.sleep(.1)

  return(model)
}
