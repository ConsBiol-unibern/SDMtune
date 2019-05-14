#' Grid Search
#'
#' Given a set of possible hyperparameter values, the function trains models
#' with all the possible combinations of hyperparameters.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperparameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \link{SWD} object. Test dataset used to evaluate the model, not
#' used with \link{aicc} and \link{SDMmodelCV} objects, default is NULL.
#' @param bg4test \link{SWD} object or NULL. Background locations used to get
#' subsamples if **a** hyperparameter is tuned, default is NULL.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, default is
#' FALSE.
#' @param save_models logical, if FALSE the models are not saved and the output
#' contains only a data frame with the metric values for each hyperparameter
#' combination. Default is TRUE, set it to FALSE when there are many
#' combinations to avoid R crashing for memory overload.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is NULL.
#'
#' @details To know which hyperparameters can be tune you can use the output of
#' the function \link{get_tunable_args}. Parallel computation increases the
#' speed only for large datasets due to the time necessary to create the
#' cluster.
#'
#' @return \link{SDMtune} object.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples \dontrun{output <- gridSearch(my_model, hypers = list( "reg" =
#' c(0.5, 1, 1.5), "fc" = c("lq", "lqp", "lqph"), "a" = c(5000, 10000, 15000)),
#' bg4test = bg, test = my_val)}
#'
#' @author Sergio Vignali
gridSearch <- function(model, hypers, metric, test = NULL, bg4test = NULL,
                       env = NULL, parallel = FALSE, save_models = TRUE,
                       seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))
  # Create a grid with all the possible combination of hyperparameters
  grid <- .get_hypers_grid(model, hypers)

  # Check that areguments are correctly provided
  .checkArgs(model, metric, test, bg4test, env, hypers)

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  pb <- progress::progress_bar$new(
    format = "Grid search [:bar] :percent in :elapsedfull",
    total = (nrow(grid) + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

  # Set bg4test as NULL if hypers doesn't contain "a"
  if (!"a" %in% names(hypers))
    bg4test <- NULL

  models <- vector("list", length = nrow(grid))
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)
  footer <- vector("character", length = nrow(grid))
  # Show line only if one hyperparameter is tuned
  show_line <- ifelse(length(hypers) == 1, TRUE, FALSE)

  if (!is.null(hypers$a)) {
    vars <- colnames(model@p@data)
    bg4test@data <- bg4test@data[vars]
    bg_folds <- sample(nrow(bg4test@data))
  } else {
    bg_folds <- NULL
  }

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

  # Loop through all the settings in grid
  for (i in 1:nrow(grid)) {

    obj <- .create_model_from_settings(model, settings = grid[i, ],
                                       bg4test = bg4test,
                                       bg_folds = bg_folds)


    train_metric[i, ] <- list(i, .get_metric(metric, obj, env = env,
                                             parallel = parallel))
    if (metric != "aicc")
      val_metric[i, ] <- list(i, .get_metric(metric, obj, test))

    if (save_models) {
      models[[i]] <- obj
    } else {
      if (i == 1) {
        output <- .create_sdmtune_output(list(obj), metric, train_metric,
                                         val_metric)
      } else {
        output@results[i, ] <- .create_sdmtune_result(obj, metric,
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
    output <- .create_sdmtune_output(models, metric, train_metric, val_metric)
  } else {
    output@models <- list(model)
  }

  pb$tick(1)

  return(output)
}
