#' Confusion Matrix
#'
#' Computes Confusion Matrixes for threshold values varying from 0 to 1
#'
#' @param model Maxent object.
#' @param test SWD test locations, if not provided it uses the train dataset,
#' default is NULL.
#' @param th vector if provided it computes the evaluation at the given thresholds,
#' default is NULL.
#'
#' @return The Confusion Matrix for all the used thresholds.
#' @export
#'
#' @examples
#' \dontrun{
#' confMatrix(my_model, test = test_dataset)}
confMatrix <- function(model, test = NULL, th = NULL) {

  if (class(model) != "Maxent")
    stop("Model must be a Maxent object!")
  if (!is.null(test) & class(test) != "SWD")
    stop("Dataset must be a SWD object!")

  if (is.null(test)) {
    p_pred <- predict(model, model@presence)
  } else {
    p_pred <- predict(model, test)
  }
  bg_pred <- predict(model, model@background)

  n_pres <- length(p_pred)
  n_bg <- length(bg_pred)

  if (is.null(th)) {
    if (n_pres > 1000) {
      th <- sample(p_pred, size = 1000)
    } else {
      th <- p_pred
    }

    if (n_bg > 1000) {
      th <- sort(unique(c(0, th, sample(bg_pred, size = 1000), 1)))
    } else {
      th <- sort(unique(c(0, th, bg_pred, 1)))
    }
  }

  tp <- fp <- vector(mode = "numeric", length = length(th))

  for (i in 1:length(th)) {
    tp[i] <- sum(p_pred >= th[i])
    fp[i] <- sum(bg_pred >= th[i])
  }

  fn <- n_pres - tp
  tn <- n_bg - fp
  conf_matrix <- data.frame(th = th, tp = tp, fp = fp, fn = fn, tn = tn)

  return(conf_matrix)
}
