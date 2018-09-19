#' Confusion Matrix
#'
#' Computes Confusion Matrixes for threshold values varying from 0 to 1.
#'
#' @param model SDMmodel object.
#' @param type character. The output type, possible values are "cloglog" and
#' "logistic", default is "cloglog".
#' @param test SWD test locations, if not provided it uses the train dataset,
#' default is NULL.
#' @param th vector if provided it computes the evaluation at the given
#' thresholds, default is NULL and it computes the evaluation for a sequence
#' from 0 to 1 with steps of 0.0001.
#'
#' @return The Confusion Matrix for all the used thresholds.
#' @export
#'
#' @examples
#' \dontrun{
#' confMatrix(my_model, type = "logisitc")}
confMatrix <- function(model, type = c("cloglog", "logistic"), test = NULL,
                       th = NULL) {

  type <- match.arg(type)

  if (class(model@model) != "Maxnet") {
    object <- model@model
  } else {
    object <- model@model@model
  }

  if (is.null(test)) {
    p_pred <- predict(object, model@presence@data, type = type)
  } else {
    p_pred <- predict(object, test@data, type = type)
  }
  bg_pred <- predict(object, model@background@data, type = type)

  n_pres <- length(p_pred)
  n_bg <- length(bg_pred)

  if (is.null(th)) {
    th <- sort(unique(c(p_pred, bg_pred)))
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
