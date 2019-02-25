#' Grid Search
#'
#' @param model
#' @param hypers
#' @param metric
#' @param test
#' @param bg4test
#' @param env
#' @param parallel
#' @param seed
#'
#' @return
#' @export
#' @importFrom progress progress_bar
#'
#' @examples
gridSearch <- function(model, hypers, metric, test = NULL, bg4test = NULL,
                       env = NULL, parallel = FALSE, seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  .checkGridSearchArgs(model, hypers, metric, test, bg4test, env)

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  grid <- .get_hypers_grid(model, hypers)

  pb <- progress::progress_bar$new(
    format = "Grid search [:bar] :percent in :elapsedfull",
    total = (nrow(grid) + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  x_title <- .get_x_grid_title(hypers)
  x_labels <- .get_x_grid_labels(hypers, grid)

  # Loop through all the settings in grid
  for (i in 1:nrow(grid)) {
    message(grid[i, ])
    pb$tick(1)
  }
  pb$tick(1)
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
}

.get_x_grid_title <- function(hypers) {
  if (length(hypers) > 1) {
    return("model")
  } else {
    return(names(hypers))
  }
}

.get_x_grid_labels <- function(hypers, grid) {
  if (length(hypers) > 1) {
    return(1:nrow(grid))
  } else {
    return(hypers[[1]])
  }
}
