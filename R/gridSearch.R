#' Grid Search
#'
#' Given a set of possible hyperparameter values, the function trains models
#' with all the possible combinations of hyperparameters.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperparameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \linkS4class{SWD} object. Testing dataset used to evaluate the
#' model, not used with \link{aicc} and \linkS4class{SDMmodelCV} objects,
#' default is `NULL`.
#' @param env \link[raster]{stack} containing the environmental variables, used
#' only with "aicc", default is `NULL`.
#' @param save_models logical, if `FALSE` the models are not saved and the
#' output contains only a data frame with the metric values for each
#' hyperparameter combination. Default is `TRUE`, set it to `FALSE` when there
#' are many combinations to avoid R crashing for memory overload.
#'
#' @details * To know which hyperparameters can be tuned you can use the output
#' of the function \link{getTunableArgs}. Hyperparameters not included in the
#' `hypers` argument take the value that they have in the passed model.
#'
#' @return \linkS4class{SDMtune} object.
#' @export
#'
#' @author Sergio Vignali
#'
#' @seealso \link{randomSearch} and \link{optimizeModel}.
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Define the hyperparameters to test
#' h <- list(reg = 1:2, fc = c("lqp", "lqph"))
#'
#' # Run the function using the AUC as metric
#' output <- gridSearch(model, hypers = h, metric = "auc", test = test)
#' output@results
#' output@models
#' # Order rusults by highest test AUC
#' head(output@results[order(-output@results$test_AUC), ])
#'
#' # Run the function using the AICc as metric and without saving the trained
#' # models, helpful when numerous hyperparameters are tested to avoid memory
#' # problems
#' output <- gridSearch(model, hypers = h, metric = "aicc", env = predictors,
#'                      save_models = FALSE)
#' output@results
#' }
gridSearch <- function(model, hypers, metric, test = NULL, env = NULL,
                       save_models = TRUE) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))
  # Create a grid with all the possible combination of hyperparameters
  grid <- .get_hypers_grid(model, hypers)

  # Check that areguments are correctly provided
  .check_args(model, metric, test, env, hypers)

  if (inherits(model, "SDMmodelCV"))
    test <- TRUE

  pb <- progress::progress_bar$new(
    format = "Grid search [:bar] :percent in :elapsedfull",
    total = (nrow(grid) + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  models <- vector("list", length = nrow(grid))
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)
  footer <- vector("character", length = nrow(grid))
  # Show line only if one hyperparameter is tuned
  show_line <- ifelse(length(hypers) == 1, TRUE, FALSE)

  # Create chart
  settings <- list(metric = .get_metric_label(metric),
                   max = nrow(grid),
                   show_line = show_line,
                   title = "Grid Search",
                   update = TRUE)

  data <- list()

  folder <- tempfile("SDMtune")

  .create_chart(folder = folder, script = "gridSearch.js", settings = settings,
                data = data)
  .show_chart(folder)

  # Loop through all the settings in grid
  for (i in seq_len(nrow(grid))) {

    obj <- .create_model_from_settings(model, settings = grid[i, ])

    train_metric[i, ] <- list(i, .get_metric(metric, obj, env = env))
    if (metric != "aicc")
      val_metric[i, ] <- list(i, .get_metric(metric, obj, test))

    if (save_models) {
      models[[i]] <- obj
    } else {
      if (i == 1) {
        o <- .create_sdmtune_output(list(obj), metric, train_metric, val_metric)
      } else {
        o@results[i, ] <- .create_sdmtune_result(obj, metric,
                                                 train_metric[i, 2],
                                                 val_metric[i, 2])
      }
    }

    footer[i] <- .get_footer(obj)
    stop <- ifelse(i == nrow(grid), TRUE, FALSE)
    .update_data(folder, data = list(train = train_metric, val = val_metric,
                                     gridFooter = footer, stop = stop))
    pb$tick(1)
  }

  if (save_models) {
    o <- .create_sdmtune_output(models, metric, train_metric, val_metric)
  } else {
    o@models <- list(model)
    if (metric == "aicc")
      o@results$delta_AICc <- o@results$AICc - min(o@results$AICc)
  }

  pb$tick(1)

  return(o)
}
