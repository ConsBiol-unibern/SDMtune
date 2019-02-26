#' Grid Search
#'
#' Given a set of possible hyperparameter values, the function trains models
#' with all the possible combinations of hyperparameters.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperpatameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \link{SWD} object. Test dataset used to evaluate the model, not
#' used with aicc and \link{SDMmodelCV} objects, default is NULL.
#' @param bg4test \link{SWD} object or NULL. Background locations used to get
#' subsamples if **a** hyperparameter is tuned, default is NULL.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is NULL.
#'
#' @details To know which hyperparameters can be tune you can use the output of
#' the function \link{get_tunable_args}.
#' You need package \pkg{snow} to use parallel computation and
#' \pkg{rgdal} to save the prediction in a raster file. Parallel computation
#' increases the speed only for big datasets due to the time necessary to create
#' the cluster.
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
                       env = NULL, parallel = FALSE, seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  # Check that areguments are correctly provided
  .checkGridSearchArgs(model, hypers, metric, test, bg4test, env)

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  # Create a grid with all the possible combination of hyperparameters
  grid <- .get_hypers_grid(model, hypers)

  pb <- progress::progress_bar$new(
    format = "Grid search [:bar] :percent in :elapsedfull",
    total = (nrow(grid) + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

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
                   labels = c(""),
                   show_line = show_line,
                   update = TRUE)

  data = list()

  folder <- tempfile("SDMtune")

  .create_chart(folder = folder, script = "gridSearch.js",
                settings = settings, data = data)

  # Loop through all the settings in grid
  for (i in 1:nrow(grid)) {
    models[[i]] <- .create_model_from_settings(model, settings = grid[i, ],
                                               bg4test = bg4test,
                                               bg_folds = bg_folds)
    train_metric[i, ] <- list(i, .get_metric(metric, models[[i]], env = env,
                                             parallel = parallel))
    if (metric != "aicc")
      val_metric[i, ] <- list(i, .get_metric(metric, models[[i]], test))
    footer[i] <- .get_footer(models[[i]])
    stop <- ifelse(i == nrow(grid), TRUE, FALSE)
    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      gridFooter = footer, stop = stop))
    Sys.sleep(.1)
    pb$tick(1)
  }
  output <- .create_sdmtune_output(models, metric, train_metric, val_metric)
  pb$tick(1)

  return(output)
}

.checkGridSearchArgs <- function(model, hypers, metric, test = NULL,
                                 bg4test = NULL, env = NULL) {
  # Throws exception if metric is aicc and env is not provided
  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  if (class(model) == "SDMmodel" & is.null(test) & metric != "aicc") {
    stop("You need to provide a test dataset!")
  }
  # Throws exception if metric is aicc and model is SDMmodelCV
  if (class(model) == "SDMmodelCV" & metric == "aicc")
    stop("Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if hypers includes 'a' and bg4test is not provided
  if (!is.null(hypers$a) & is.null(bg4test))
    stop("bg4test must be provided to tune background locations!")
  # Throws exception if max hypers 'a' > than nrow bg4test
  if (!is.null(hypers$a)) {
    if (max(hypers$a) > nrow(bg4test@data))
      stop(paste0("Maximum number of 'a' hyperparameter cannot be more than ",
                  nrow(bg4test@data), "!"))
  }
  # Throws exception if provided hypers are not tunable
  diff <- setdiff(names(hypers), get_tunable_args(model))
  if (length(diff) > 0)
    stop(paste(diff, "non included in tunable hyperparameters",
               collapse = ", "))
}
