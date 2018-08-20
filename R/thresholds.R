#' Thresholds
#'
#' Compute three threshold values: minimum training presence, equal training
#' sensitivity and specificity and maximum training sensitivity plus
#' specificity.
#'
#' @param model SDMmodel object
#' @param type character. The output type, possible values are "cloglog" and
#' "logistic", default is "cloglog".
#'
#' @details The equal training sensitivity and specificity minimizes the
#' difference between sensitivity and specificity.
#'
#' @return data.frame with the thresholds.
#' @export
#'
#' @examples
#' \dontrun{thresholds(model, type = "logistic")}
#'
#' @author Sergio Vignali
thresholds <- function(model, type) {

  if (class(model@model) == "Maxent") {
    object <- model@model
  } else {
    object <- model@model@model
  }

  cm <- confMatrix(model, type = type)
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)

  mtp <- round(min(predict(object, model@presence@data, type = type)), 4)
  ess <- round(cm$th[which.min(abs(tpr - tnr))], 4)
  mss <- round(cm$th[which.max(tpr + tnr)], 4)

  ths <- data.frame(th = c(mtp, ess, mss))
  rownames(ths) <- c("Minimum training presence",
                     "Equal training sensitivity and specificity",
                     "Maximum training sensitivity plus specificity")

  return(ths)
}
