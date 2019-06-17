#' Confusion Matrix
#'
#' Computes Confusion Matrixes for threshold values varying from 0 to 1.
#'
#' @param model \link{SDMmodel} object.
#' @param type character. The output type, possible values are "cloglog" and
#' "logistic", default is "cloglog".
#' @param test \link{SWD} test locations, if not provided it uses the train
#' dataset, default is NULL.
#' @param th numeric vector, if provided it computes the evaluation at the given
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

  if (is.null(test)) {
    p <- model@p@data
  } else {
    p <- test@data[colnames(model@p@data)]
  }
  a <- model@a@data

  n_p <- nrow(p)
  n_a <- nrow(a)
  pred <- predict(model, rbind(p, a), type = type)
  p_pred <- pred[1:n_p]
  a_pred <- pred[(n_p + 1):(n_p + n_a)]

  if (is.null(th)) {
    th <- sort(unique(c(p_pred, a_pred)))
  }

  tp <- fp <- vector(mode = "numeric", length = length(th))

  for (i in 1:length(th)) {
    tp[i] <- sum(p_pred >= th[i])
    fp[i] <- sum(a_pred >= th[i])
  }

  fn <- n_p - tp
  tn <- n_a - fp
  conf_matrix <- data.frame(th = th, tp = tp, fp = fp, fn = fn, tn = tn)

  return(conf_matrix)
}
