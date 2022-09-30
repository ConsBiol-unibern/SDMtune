#' Thresholds
#'
#' Compute three threshold values: minimum training presence, equal training
#' sensitivity and specificity and maximum training sensitivity plus
#' specificity together with fractional predicted area and the omission rate. If
#' a test dataset is provided it returns also the equal test sensitivity and
#' specificity and maximum test sensitivity plus specificity thresholds and the
#' p-values of the one-tailed binomial exact test.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic".
#' @param test \linkS4class{SWD} testing locations, if not provided it returns
#' the training and test thresholds.
#'
#' @details The equal training sensitivity and specificity minimizes the
#' difference between sensitivity and specificity. The one-tailed binomial test
#' checks that test points are predicted no better than by a random prediction
#' with the same fractional predicted area.
#'
#' @return data.frame with the thresholds.
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          test = 0.2,
#'                          only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = train,
#'                fc = "l")
#'
#' # Get the cloglog thresholds
#' thresholds(model,
#'            type = "cloglog")
#'
#' # Get the logistic thresholds passing the test dataset
#' thresholds(model,
#'            type = "logistic",
#'            test = test)
thresholds <- function(model,
                       type = NULL,
                       test = NULL) {

  n_pres <- sum(model@data@pa == 1)

  cm_train <- confMatrix(model, type = type)
  tpr <- cm_train$tp / (cm_train$tp + cm_train$fn)
  tnr <- cm_train$tn / (cm_train$fp + cm_train$tn)
  fpr <- cm_train$fp / (cm_train$fp + cm_train$tn)

  mtp <- min(predict(model, .get_presence(model@data), type = type))
  ess <- cm_train$th[which.min(abs(tpr - tnr))]
  mss <- cm_train$th[which.max(tpr + tnr)]

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

    ess <- cm_test$th[which.min(abs(tpr_test - tnr_test))]
    mss <- cm_test$th[which.max(tpr_test + tnr_test)]

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

  for (i in seq_along(ths)) {
    index <- which.min(abs(cm_train$th - ths[i]))
    or_train[i] <- cm_train[index, ]$fn / n_pres
    fpa[i] <- fpr[index]

    if (!is.null(test)) {
      index <- which.min(abs(cm_test$th - ths[i]))
      or_test[i] <- cm_test[index, ]$fn / n_test
      p_values[i] <- stats::binom.test((round((1 - or_test[i]), 0) * n_test),
                                       n_test, fpa[i],
                                       alternative = "greater")$p.value
    }
  }

  output <- data.frame(th = rownames, val = ths, fpa = fpa, or = or_train,
                       stringsAsFactors = FALSE)

  if (!is.null(test)) {
    output$or_test <- or_test
    output$pv <- p_values
  }

  colnames(output) <- colnames

  output
}
