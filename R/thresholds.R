#' Thresholds
#'
#' Compute three threshold values: minimum training presence, equal training
#' sensitivity and specificity and maximum training sensitivity plus
#' specificity.
#'
#' @param model SDMmodel object
#' @param type character. The output type, possible values are "cloglog" and
#' "logistic", default is "cloglog".
#' @param test SWD test locations, if not provided it returns the training and
#' test thresholds, default is NULL.
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
thresholds <- function(model, type, test = NULL) {

  if (class(model@model) == "Maxent") {
    object <- model@model
  } else {
    object <- model@model@model
  }

  cm <- confMatrix(model, type = type)
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)

  mtp <- round(min(predict(object, model@presence@data, type = type)), 3)
  ess <- round(cm$th[which.min(abs(tpr - tnr))], 3)
  mss <- round(cm$th[which.max(tpr + tnr)], 3)

  ths <- c(mtp, ess, mss)
  rownames <- c("Minimum training presence",
                "Equal training sensitivity and specificity",
                "Maximum training sensitivity plus specificity")

  if (!is.null(test)) {
    cm <- confMatrix(model, type = type, test = test)
    tpr <- cm$tp / (cm$tp + cm$fn)
    tnr <- cm$tn / (cm$fp + cm$tn)

    mtp <- round(min(predict(object, model@presence@data, type = type)), 3)
    ess <- round(cm$th[which.min(abs(tpr - tnr))], 3)
    mss <- round(cm$th[which.max(tpr + tnr)], 3)

    ths <- c(ths, mtp, ess, mss)
    rownames <- c(rownames,
                  "Minimum test presence",
                  "Equal test sensitivity and specificity",
                  "Maximum test sensitivity plus specificity")
  }

  output <- data.frame(Threshold = rownames, Values = ths)
  colnames(output) <- c("Threshold",
                        paste(stringr::str_to_title(type), "value"))

  return(output)
}
