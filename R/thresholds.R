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

  n_pres <- nrow(model@presence@data)

  cm_train <- confMatrix(model, type = type)
  tpr <- cm_train$tp / (cm_train$tp + cm_train$fn)
  tnr <- cm_train$tn / (cm_train$fp + cm_train$tn)

  mtp <- round(min(predict(object, model@presence@data, type = type)), 3)
  ess <- round(cm_train$th[which.min(abs(tpr - tnr))], 3)
  mss <- round(cm_train$th[which.max(tpr + tnr)], 3)

  ths <- c(mtp, ess, mss)
  rownames <- c("Minimum training presence",
                "Equal training sensitivity and specificity",
                "Maximum training sensitivity plus specificity")
  colnames <- c("Threshold",
                paste(stringr::str_to_title(type), "value"),
                "Training omission rate")

  if (!is.null(test)) {
    cm_test <- confMatrix(model, type = type, test = test)
    tpr <- cm_test$tp / (cm_test$tp + cm_test$fn)
    tnr <- cm_test$tn / (cm_test$fp + cm_test$tn)

    ess <- round(cm_test$th[which.min(abs(tpr - tnr))], 3)
    mss <- round(cm_test$th[which.max(tpr + tnr)], 3)

    ths <- c(ths, ess, mss)
    rownames <- c(rownames,
                  "Equal test sensitivity and specificity",
                  "Maximum test sensitivity plus specificity")
    colnames <- c(colnames, "Test omission rate")
    n_test <- nrow(test@data)
    or_test <- vector(mode = "numeric", length = 5)
  }
  or_train <- vector(mode = "numeric", length = length(ths))
  output <- data.frame(Threshold = rownames, Values = ths)
  for (i in 1:length(ths)) {
    index <- which.min(abs(cm_train$th - output[i, 2]))
    or_train[i] <- round(cm_train[index, ]$fn / n_pres, 3)
    if (!is.null(test)) {
      index <- which.min(abs(cm_test$th - output[i, 2]))
      or_test[i] <- round(cm_test[index, ]$fn / n_test, 3)
    }
  }
  output$or <- or_train
  if (!is.null(test))
    output$or_test <- or_test

  colnames(output) <- colnames

  return(output)
}
