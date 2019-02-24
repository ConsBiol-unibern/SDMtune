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

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel" & is.null(test) & metric != "aicc") {
    stop("You need to provide a test dataset!")
  }
  if (class(model) == "SDMmodelCV") {
    test <- TRUE
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }
  if (!is.null(hypers$a) & is.null(bg4test))
    stop("bg4test must be provided to tune background locations!")
  if (!is.null(hypers$a) & max(hypers$a) > nrow(bg4test@data))
    stop(paste("Maximum number of 'a' hyperparameter cannot be more than!",
               nrow(bg4test@data)))

  # Create data frame with all possible combinations of hyperparameters
  tunable_args <- .get_train_args(model)[get_tunable_args(model)]
  tunable_args[names(hypers)] <- hypers
  if (is.null(hypers$a))
    tunable_args$a <- nrow(model@a@data)
  grid <- expand.grid(tunable_args, stringsAsFactors = FALSE)

  pb <- progress::progress_bar$new(
    format = "Grid search [:bar] :percent in :elapsedfull",
    total = (nrow(grid) + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  # Loop through all the settings in grid
  for (i in 1:nrow(grid)) {
    message(grid[i, ])
  }
}
