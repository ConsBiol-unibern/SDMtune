#' Confusion Matrix
#'
#' Computes Confusion Matrixes for threshold values varying from 0 to 1.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param test \linkS4class{SWD} testing locations, if not provided it uses the
#' training dataset.
#' @param th numeric vector. If provided it computes the evaluation at the given
#' thresholds. Default is `NULL` and it computes the evaluation for the unique
#' predicted values at presence and absence/background locations.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic".
#'
#' @details
#' * For models trained with the **Maxent** method the argument `type` can be:
#' "raw", "logistic" and "cloglog".
#' * For models trained with the **Maxnet** method the argument `type` can be:
#' "link", "exponential", "logistic" and "cloglog", see \link[maxnet]{maxnet}
#' for more details.
#'
#' @return The Confusion Matrix for all the used thresholds.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
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
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l")
#'
#' # Get the confusion matrix for thresholds ranging from 0 to 1
#' cm <- confMatrix(model,
#'                  type = "cloglog")
#' head(cm)
#' tail(cm)
#'
#' # Get the confusion matrix for a specific threshold
#' confMatrix(model,
#'            type = "logistic",
#'            th = 0.6)
confMatrix <- function(model,
                       test = NULL,
                       th = NULL,
                       type = NULL) {

  if (!inherits(model, "SDMmodel"))
    cli::cli_abort(c(
      "!" = "Function available only for {.cls SDMmodel} objects.",
      "x" = "You have supplied a {.cls {class(model)}} instead."))

  if (is.null(test)) {
    data <- model@data
  } else {
    if (!inherits(test, "SWD"))
      cli::cli_abort(c(
        "!" = "{.var test} must be an {.cls SWD} object",
        "x" = "You have supplied a {.cls {class(test)}} instead."
      ))

    data <- test
  }

  n_p <- sum(data@pa == 1)
  n_a <- sum(data@pa == 0)
  pred <- predict(model,
                  data,
                  type = type)
  p_pred <- pred[1:n_p]
  a_pred <- pred[(n_p + 1):(n_p + n_a)]

  if (is.null(th)) {
    th <- sort(unique(pred))
    th <- c(0, th, 1)
  }

  tp <- fp <- vector(mode = "numeric", length = length(th))

  for (i in seq_along(th)) {
    tp[i] <- sum(p_pred >= th[i])
    fp[i] <- sum(a_pred >= th[i])
  }

  fn <- n_p - tp
  tn <- n_a - fp

  data.frame(th = th,
             tp = tp,
             fp = fp,
             fn = fn,
             tn = tn)

}
