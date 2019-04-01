#' Reduce Variables
#'
#' Remove variables whose permutation importance is less than the
#' given threshold. The function removes one variable at time and after trains a
#' new model to get the new variable contribution rank. If use_jk is TRUE the
#' function checks if after removing the variable the model performance
#' decreases (according to the given metric and based on the starting model). In
#' this case the function stops removing the variable even if the contribution
#' is lower than the given threshold.
#'
#' @param model S\link{DMmodel} or \link{SDMmodelCV} object.
#' @param th numeric. The contribution threshold used to remove variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", used only if use_jk is TRUE.
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
#' @param use_pc logical, use percent contribution. If TRUE and the model is
#' trained using the \link{Maxent} method, the algorithm uses the percent
#' contribution computed by Maxent software to score the varialble importance,
#' default is FALSE.
#'
#' @return The model trained using the selected variables.
#' @export
#'
#' @examples
#' \dontrun{model <- reduveVar(model = model, th = 2)}
#'
#' @author Sergio Vignali
reduceVar <- function(model, th, metric, test = NULL, env = NULL,
                      parallel = FALSE, use_jk = FALSE, permut = 10,
                      use_pc = FALSE) {

  metric <- match.arg(metric, c("auc", "tss", "aicc"))

  if (use_jk == TRUE & is.null(test) & metric != "aicc")
    stop("You need to provide a test dataset!")
  if (use_jk == TRUE & metric == "aicc" & is.null(env))
    stop("You must provide the env argument if you want to use AICc metric!")
  if (use_pc & .get_model_class(model) != "Maxent")
    warning(paste("Percent contribution cannot be used with model of method",
                  .get_model_class(model)))

  variables_reduced <- FALSE
  first_iter <- TRUE
  removed_vars <- c()

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
  line_title <- "Starting model"
  line_footer <- ""
  settings <- list(labels = initial_vars, metric = .get_metric_label(metric),
                   title = "Reduce Variable", update = TRUE)
  folder <- tempfile("SDMsel")

  while (variables_reduced == FALSE) {

    continue_jk <- FALSE

    if (use_pc) {
      scores <- maxentVarImp(model)
    } else {
      scores <- suppressMessages(varImp(model, permut = permut))
    }

    # Update chart
    index <- match(initial_vars, scores[, 1])
    vals <- scores[, 2][index]
    vals[is.na(vals)] <- 0
    data = list(data = vals, train = train_metric, val = val_metric,
                lineTitle = line_title, lineFooter = line_footer, stop = FALSE)

    if (first_iter) {
      .create_chart(folder = folder, script = "varSelection.js",
                    settings = settings, data = data, height = 600)
      first_iter = FALSE
    } else {
      .update_chart(folder, data = data)
    }
    Sys.sleep(.1)

    scores <- scores[order(scores[, 2]), ]
    scores <- scores[scores[, 2] <= th, ]

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

          if (metric  != "aicc") {
            if (jk_test$results[1, 3] >= val_metric[1, 2]) {
              model <- jk_test$models_without[[1]]
              train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])
              val_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 3])
              continue_jk <- TRUE
              line_title <- c(line_title, paste("Removed", scores[i, 1]))
              line_footer <- c(line_footer, "")
              removed_vars <- c(removed_vars, scores[i, 1])
              break
            }
          } else {
            if (jk_test$results[1, 2] <= train_metric[1, 2]) {
              model <- jk_test$models_without[[1]]
              train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])
              continue_jk <- TRUE
              line_title <- c(line_title, paste("Removed", scores[i, 1]))
              line_footer <- c(line_footer, "")
              removed_vars <- c(removed_vars, scores[i, 1])
              break
            }
          }
        }
        if (continue_jk) {
          next
        } else {

          if (use_pc) {
            scores <- maxentVarImp(model)
          } else {
            scores <- suppressMessages(varImp(model, permut = permut))
          }

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
        line_title <- c(line_title, paste("Removed", scores[1, 1]))
        line_footer <- c(line_footer, "")
        removed_vars <- c(removed_vars, scores[1, 1])
      }
    } else {
      variables_reduced <- TRUE
    }
  }

  .update_chart(folder, data = list(data = vals, train = train_metric,
                                    val = val_metric, drawLine1 = FALSE,
                                    lineTitle = line_title,
                                    lineFooter = line_footer, stop = TRUE))
  Sys.sleep(.1)

  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
