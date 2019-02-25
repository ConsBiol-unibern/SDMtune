.get_hypers_grid <- function(model, hypers) {
  # Create data frame with all possible combinations of hyperparameters
  tunable_args <- .get_train_args(model)[get_tunable_args(model)]
  tunable_args[names(hypers)] <- hypers
  if (is.null(hypers$a))
    tunable_args$a <- nrow(model@a@data)
  grid <- expand.grid(tunable_args, stringsAsFactors = FALSE)
  return(grid)
}
