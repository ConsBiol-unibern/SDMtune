#' Thresholds
#'
#' Compute three threshold values: minimum training presence, equal training
#' sensitivity and specificity and maximum training sensitivity plus
#' specificity together with fractional predicted area and the omission rate. If
#' a test dataset is provided it returns also the equal test sensitivity and
#' specificity and maximum test sensitivity plus specificity thresholds and the
#' p-values of the one-tailed binomial exact test.
#'
#' @param model SDMmodel object
#' @param type character. The output type, possible values are "cloglog" and
#' "logistic", default is "cloglog".
#' @param test SWD test locations, if not provided it returns the training and
#' test thresholds, default is NULL.
#'
#' @details The equal training sensitivity and specificity minimizes the
#' difference between sensitivity and specificity. The one-tailed binomial test
#' checks that test points are predicted no better than by a random prediction
#' with the same fractional predicted area.
#'
#' @return data.frame with the thresholds.
#' @export
#' @importFrom stats binom.test
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
  fpr <- cm_train$fp / (cm_train$fp + cm_train$tn)

  mtp <- round(min(predict(object, model@presence@data, type = type)), 3)
  ess <- round(cm_train$th[which.min(abs(tpr - tnr))], 3)
  mss <- round(cm_train$th[which.max(tpr + tnr)], 3)

  ths <- c(mtp, ess, mss)
  rownames <- c("Minimum training presence",
                "Equal training sensitivity and specificity",
                "Maximum training sensitivity plus specificity")
  colnames <- c("Threshold",
                paste(stringr::str_to_title(type), "value"),
                "Fractional predicted area",
                "Training omission rate")

  if (!is.null(test)) {
    cm_test <- confMatrix(model, type = type, test = test)
    tpr_test <- cm_test$tp / (cm_test$tp + cm_test$fn)
    tnr_test <- cm_test$tn / (cm_test$fp + cm_test$tn)

    ess <- round(cm_test$th[which.min(abs(tpr_test - tnr_test))], 3)
    mss <- round(cm_test$th[which.max(tpr_test + tnr_test)], 3)

    ths <- c(ths, ess, mss)
    rownames <- c(rownames,
                  "Equal test sensitivity and specificity",
                  "Maximum test sensitivity plus specificity")
    colnames <- c(colnames, "Test omission rate", "P-values")
    n_test <- nrow(test@data)
    or_test <- vector(mode = "numeric", length = 5)
    p_values <- vector(mode = "numeric", length = 5)
  }
  or_train <- vector(mode = "numeric", length = length(ths))
  fpa <- vector(mode = "numeric", length = length(ths))

  for (i in 1:length(ths)) {
    index <- which.min(abs(cm_train$th - ths[i]))
    or_train[i] <- round(cm_train[index, ]$fn / n_pres, 3)
    fpa[i] <- round(fpr[index], 3)
    if (!is.null(test)) {
      index <- which.min(abs(cm_test$th - ths[i]))
      or_test[i] <- round(cm_test[index, ]$fn / n_test, 3)
      p_values[i] <- stats::binom.test((round((1 - or_test[i]), 0) * n_test),
                                       n_test, fpa[i],
                                       alternative = "greater")$p.value
    }
  }

  output <- data.frame(th = rownames, val = ths, fpa = fpa, or = or_train)

  if (!is.null(test)) {
    output$or_test <- or_test
    output$pv <- p_values
  }

  colnames(output) <- colnames

  return(output)
}
