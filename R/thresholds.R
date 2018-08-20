#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
thresholds <- function(model) {

  if (class(model@model) == "Maxent") {
    object <- model@model
  } else {
    object <- model@model@model
  }

  cm <- confMatrix(model)
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)

  mtp <- min(predict(object, model@presence@data))
  ess <- min(abs(tpr - tnr))
  mss <- max(tpr + tnr)

  ths <- c(mtp, ess, mss)
  names(ths) <- c("Minimum training presence",
                  "Equal training sensitivity and specificity",
                  "Maximum training sensitivity plus specificity")

  return(ths)
}
