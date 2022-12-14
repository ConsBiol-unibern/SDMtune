#' Reduce Variables
#'
#' Remove variables whose importance is less than the given threshold. The
#' function removes one variable at time and after trains a new model to get the
#' new variable contribution rank. If use_jk is TRUE the function checks if
#' after removing the variable the model performance decreases (according to the
#' given metric and based on the starting model). In this case the function
#' stops removing the variable even if the contribution is lower than the given
#' threshold.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param th numeric. The contribution threshold used to remove variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", used only if use_jk is `TRUE`.
#' @param test \linkS4class{SWD} object containing the test dataset used to
#' evaluate the model, not used with aicc, and if `use_jk = FALSE`.
#' @param env \link[terra]{rast} containing the environmental variables, used
#' only with "aicc".
#' @param use_jk Flag to use the Jackknife AUC test during the variable
#' selection, if `FALSE` the function uses the percent variable contribution.
#' @param permut integer. Number of permutations, used if `use_pc = FALSE`.
#' @param use_pc logical. If `TRUE` and the model is trained using the
#' \linkS4class{Maxent} method, the algorithm uses the percent contribution
#' computed by Maxent software to score the variable importance.
#' @param interactive logical. If `FALSE` the interactive chart is not created.
#' @param verbose logical. If `TRUE` prints informative messages.
#'
#' @details An interactive chart showing in real-time the steps performed by the
#' algorithm is displayed in the Viewer pane.
#'
#' @return The model trained using the selected variables.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          test = 0.2,
#'                          only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a Maxnet model
#' model <- train(method = "Maxnet",
#'                data = train,
#'                fc = "lq")
#'
#' # Remove all variables with permuation importance lower than 2%
#' output <- reduceVar(model,
#'                     th = 2,
#'                     metric = "auc",
#'                     test = test,
#'                     permut = 1)
#'
#' # Remove variables with permuation importance lower than 3% only if testing
#' # TSS doesn't decrease
#' \dontrun{
#' output <- reduceVar(model,
#'                     th = 3,
#'                     metric = "tss",
#'                     test = test,
#'                     permut = 1,
#'                     use_jk = TRUE)
#'
#' # Remove variables with permuation importance lower than 2% only if AICc
#' # doesn't increase
#' output <- reduceVar(model,
#'                     th = 2,
#'                     metric = "aicc",
#'                     permut = 1,
#'                     use_jk = TRUE,
#'                     env = predictors)
#'
#' # Train a Maxent model
#' model <- train(method = "Maxent",
#'                data = train,
#'                fc = "lq")
#'
#' # Remove all variables with percent contribution lower than 2%
#' output <- reduceVar(model,
#'                     th = 2,
#'                     metric = "auc",
#'                     test = test,
#'                     use_pc = TRUE)
#' }
#' }
reduceVar <- function(model,
                      th,
                      metric,
                      test = NULL,
                      env = NULL,
                      use_jk = FALSE,
                      permut = 10,
                      use_pc = FALSE,
                      interactive = TRUE,
                      verbose = TRUE) {

  metric <- match.arg(metric, c("auc", "tss", "aicc"))

  if (use_jk == TRUE)
    .check_args(model, metric = metric, test = test, env = env)

  if (use_pc & .get_model_class(model) != "Maxent")
    cli::cli_abort(paste("Percent contribution cannot be used with",
                         "a {.cls { .get_model_class(model) }} model."))

  if (!is.null(env)) {
    # TODO: Remove with version 2.0.0
    if (inherits(env, "Raster")) {
      .warn_raster("raster", "rast")
      env <- terra::rast(env)
    }

    if (!inherits(env, "SpatRaster"))
      cli::cli_abort(c(
        "!" = "{.var env} must be a {.cls SpatRaster} object",
        "x" = "You have supplied a {.cls {class(env)}} instead."
      ))
  }

  if (inherits(model, "SDMmodelCV"))
    test <- TRUE

  variables_reduced <- FALSE
  first_iter <- TRUE
  removed_vars <- character()

  # Metrics are used also in if statements outside chart
  train_metric <- data.frame(x = 0, y = .get_metric(metric, model, env = env))
  if (metric != "aicc") {
    val_metric <- data.frame(x = 0, y = .get_metric(metric, model, test = test))
  } else {
    val_metric <- data.frame(x = NA_real_, y = NA_real_)
  }

  if (interactive) {
    # Create chart
    initial_vars <- colnames(model@data@data)
    line_title <- "Starting model"
    line_footer <- ""
    settings <- list(labels = initial_vars, metric = .get_metric_label(metric),
                     title = "Reduce Variables", update = TRUE)
    folder <- tempfile("SDMtune-reduceVar")
  }

  while (variables_reduced == FALSE) {

    continue_jk <- FALSE

    if (use_pc) {
      scores <- maxentVarImp(model)
    } else {
      scores <- varImp(model, permut = permut, progress = FALSE)
    }

    if (interactive) {
      # Update chart
      index <- match(initial_vars, scores[, 1])
      vals <- scores[, 2][index]
      vals[is.na(vals)] <- 0
      data <- list(data = vals, train = train_metric, val = val_metric,
                   lineTitle = line_title, lineFooter = line_footer, stop = FALSE)

      if (first_iter) {
        .create_chart(folder = folder, script = "varSelection.js",
                      settings = settings, data = data)
        .show_chart(folder, height = "maximize")
        first_iter <- FALSE
      } else {
        .update_data(folder, data = data)
      }
    }

    scores <- scores[order(scores[, 2]), ]
    scores <- scores[scores[, 2] <= th, ]

    if (nrow(scores) > 0) {
      if (use_jk) {
        for (i in seq_len(nrow(scores))) {
          jk_test <- doJk(model,
                          variables = as.character(scores[i, 1]),
                          metric = metric,
                          test = test,
                          with_only = FALSE,
                          return_models = TRUE,
                          env = env,
                          progress = FALSE)

          # index for metric data frames
          x <- nrow(train_metric) + 1

          if (metric  != "aicc") {
            if (jk_test$results[1, 3] >= val_metric[1, 2]) {
              model <- jk_test$models_without[[1]]
              continue_jk <- TRUE
              removed_vars <- c(removed_vars, scores[i, 1])
              train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])
              val_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 3])

              if (interactive) {
                line_title <- c(line_title, paste("Removed", scores[i, 1]))
                line_footer <- c(line_footer, "")
              }

              break
            }
          } else {
            if (jk_test$results[1, 2] <= train_metric[1, 2]) {
              model <- jk_test$models_without[[1]]
              continue_jk <- TRUE
              removed_vars <- c(removed_vars, scores[i, 1])
              train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])

              if (interactive) {
                line_title <- c(line_title, paste("Removed", scores[i, 1]))
                line_footer <- c(line_footer, "")
              }

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
            scores <- varImp(model, permut = permut, progress = FALSE)
          }

          if (interactive) {
          # Update chart
            index <- match(initial_vars, scores[, 1])
            vals <- scores[, 2][index]
            vals[is.na(vals)] <- 0
            data <- list(data = vals, train = train_metric, val = val_metric,
                         stop = FALSE)
          }
          variables_reduced <- TRUE
        }
      } else {
        jk_test <- doJk(model,
                        variables = as.character(scores[1, 1]),
                        metric = metric,
                        test = test,
                        with_only = FALSE,
                        return_models = TRUE,
                        env = env,
                        progress = FALSE)
        model <- jk_test$models_without[[1]]
        removed_vars <- c(removed_vars, scores[1, 1])
        x <- nrow(train_metric) + 1
        train_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 2])
        if (metric != "aicc")
          val_metric[x, ] <- list(x = x - 1, y = jk_test$results[1, 3])

        if (interactive) {
          # index for metric data frames
          line_title <- c(line_title, paste("Removed", scores[1, 1]))
          line_footer <- c(line_footer, "")
        }
      }
    } else {
      variables_reduced <- TRUE
    }
  }

  if (interactive) {
    .update_data(folder, data = list(data = vals, train = train_metric,
                                     val = val_metric, drawLine1 = FALSE,
                                     lineTitle = line_title,
                                     lineFooter = line_footer, stop = TRUE))
  }

  if (verbose)
    cli::cli_alert_success(paste(
      "{?No/The/The} variable{?s} {.field {removed_vars}} {?has/have}",
      "been removed"))

  model
}
