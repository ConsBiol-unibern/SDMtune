#' Confusion Matrix
#'
#' Compute the Confusion Matrix for threshold values varying from 0 to 1
#'
#' @param model Maxent object.
#' @param type character Prediction type, possible values are "cloglog" or "logistic",
#' default is "cloglog".
#' @param presence SWD presence locations, if not provided it uses the train dataset,
#' default is NULL.
#' @param th numeric if provided it computes the evaluation at the given threshold,
#' default is NULL.
#'
#' @return The Confusion Matrix for all the used thresholds.
#' @export
#'
#' @examples
#' \dontrun{
#' confMatrix(my_model, presence = test_dataset)}
confMatrix <- function(model, type = c("cloglog", "logistic"), presence = NULL,
                       th = NULL) {

  if (class(model) != "Maxent")
    stop("Model must be a Maxent object!")

  type <- match.arg(type)

  if (is.null(presence)) {
    p_pred <- predict(model, model@presence, maxent_output = type)
  } else {
    p_pred <- predict(model, presence, maxent_output = type)
  }
  bg_pred <- predict(model, model@background, maxent_output = type)

  n_pres <- nrow(p_pred)
  n_bg <- nrow(bg_pred)

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

  tp <- fp <- fn <- tn <- vector(mode = "numeric", length = length(th))

  for (i in 1:length(th)) {
    tp[i] <- sum(p_pred >= th[i])   # true positives
    fp[i] <- sum(bg_pred >= th[i])  # false positives
    fn[i] <- n_pres - tp[i]         # false negatives
    tn[i] <- n_bg - fp[i]           # true negatives
  }
  conf_matrix <- data.frame(th = th, tp = tp, fp = fp, fn = fn, tn = tn)

  return(conf_matrix)
}
