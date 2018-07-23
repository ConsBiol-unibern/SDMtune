#' Jackknife Test
#'
#' Run the Jackknife test for variable importance removing one variable at time and returns the train AUC.
#' If the model has a test dataset, it returns also the test AUC.
#'
#' @param model Maxent object..
#' @param variables vector. Variables used for the test, if not provided it takes all the variables
#' used to train the model, default is NULL.
#' @param with_only logical. If TRUE it runs the test also for each variable in isolation, default is TRUE.
#' @param return_models logical, If TRUE returns all the models together with the test result, default is FALSE.
#'
#' @return A data frame with the test results. If **return_model = TRUE** it returns a list with
#' the data frame together with the models.
#' @export
#' @importFrom progress progress_bar
#' @importFrom foreach foreach %do%
#'
#' @examples \dontrun{
#' doJk(model, variable = c('bio1', 'bio12'), with_only = TRUE)}
#'
#' @author Sergio Vignali
doJk <- function(model, variables = NULL, with_only = TRUE,
                 return_models = FALSE) {

  if (class(model) != "Maxent")
    stop("Model must be a Maxent object!")

  if (is.null(variables))
    variables <- colnames(model@presence@data)

  if (with_only) {
    tot <- length(variables) * 2
  } else {
    tot <- length(variables)
  }

  pb <- progress::progress_bar$new(
    format = "Jk Test [:bar] :percent in :elapsedfull",
    total = tot, clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  aucs_without <- aucs_withonly <- vector(mode = "numeric",
                                          length = length(variables))
  models_without <- models_withonly <- c()

  if (nrow(model@test@data) > 0) {
    aucs_test_without <- vector(mode = "numeric", length = length(variables))
    aucs_test_withonly <- vector(mode = "numeric", length = length(variables))
    auc_test <- TRUE
  } else {
    auc_test <- FALSE
  }

  foreach::foreach(i = 1:length(variables)) %do% {
    presence <- model@presence
    bg <- model@background
    presence@data[variables[i]] <- NULL
    bg@data[variables[i]] <- NULL

    if (auc_test) {
      test4jk <- model@test
      test4jk@data[variables[i]] <- NULL
    } else {
      test4jk <- NULL
    }

    jk_model <- trainMaxent(presence, bg, rm = model@rm, fc = model@fc,
                            type = model@type, test = test4jk)
    aucs_without[i] <- jk_model@results["Training.AUC", ]
    if (auc_test)
      aucs_test_without[i] <- jk_model@results["Test.AUC", ]
    models_without <- c(models_without, jk_model)
    pb$tick()

    if (with_only) {
      presence <- model@presence
      bg <- model@background
      presence@data <- presence@data[variables[i]]
      bg@data <- bg@data[variables[i]]

      if (auc_test) {
        test4jk <- model@test
        test4jk@data <- model@test@data[variables[i]]
      } else {
        test4jk <- NULL
      }

      jk_model <- trainMaxent(presence, bg, rm = model@rm, fc = model@fc,
                              type = model@type, test = test4jk)
      aucs_withonly[i] <- jk_model@results["Training.AUC", ]
      if (auc_test)
        aucs_test_withonly[i] <- jk_model@results["Test.AUC", ]
      models_withonly <- c(models_withonly, jk_model)
      pb$tick()
    }
  }

  jk_test <- data.frame(Variables = variables, Train_AUC_without = aucs_without)

  if (auc_test)
    jk_test$Test_AUC_without <- aucs_test_without
  if (with_only) {
    jk_test$Train_AUC_withonly <- aucs_withonly
    if (auc_test) jk_test$Test_AUC_withonly <- aucs_test_withonly
  }

  if (return_models) {
    if (with_only) {
      output <- list(jk_test, models_without, models_withonly)
      names(output) <- c("results", "models_without", "models_withonly")
    } else {
      output <- list(jk_test, models_without)
      names(output) <- c("results", "models_without")
    }

  } else {
    output <- jk_test
  }

  return(output)
}
