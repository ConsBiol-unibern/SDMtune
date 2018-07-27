#' Jackknife Test
#'
#' Run the Jackknife test for variable importance removing one variable at time and returns the train AUC.
#' If the model has a test dataset, it returns also the test AUC.
#'
#' @param model Maxent object.
#' @param metric character. The metric used to evaluate the models, possible values are:
#' "auc", "tss" and "aicc", default is "auc".
#' @param variables vector. Variables used for the test, if not provided it takes all the variables
#' used to train the model, default is NULL.
#' @param with_only logical. If TRUE it runs the test also for each variable in isolation, default is TRUE.
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' used only with "aicc", default is NULL.
#' @param return_models logical, If TRUE returns all the models together with the test result, default is FALSE.
#'
#' @details You need package \pkg{snow} to use parallel computation and \pkg{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
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
doJk <- function(model, metric = c("auc", "tss", "aicc"), variables = NULL,
                 with_only = TRUE, env = NULL, parallel = FALSE,
                 return_models = FALSE) {

  if (metric == "aicc" & is.null(env))
    stop("You must provide env parameter if you want to use AICc metric!")

  if (is.null(variables))
    variables <- colnames(model@presence@data)

  if (with_only) {
    tot <- length(variables) * 2
  } else {
    tot <- length(variables)
  }

  pb <- progress::progress_bar$new(
    format = "Jk Test [:bar] :percent in :elapsedfull", total = tot,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  metric <- match.arg(metric)
  models_without <- models_withonly <- c()

  res <- matrix(nrow = length(variables), ncol = 5)
  if (metric == "auc") {
    labels <- c("Variable", "Train_AUC_without", "Train_AUC_withonly",
                "Test_AUC_without", "Test_AUC_withonly")
  } else if (metric == "tss") {
    labels <- c("Variable", "Train_TSS_without", "Train_TSS_withonly",
                "Test_TSS_without", "Test_TSS_withonly")
  } else {
    labels <- c("Variable", "AICc_without", "AICc_withonly", "-", "-")
  }

  if (nrow(model@test@data) > 0 & metric != "aicc") {
    test <- TRUE
  } else {
    test <- FALSE
  }

  foreach::foreach(i = 1:length(variables)) %do% {
    presence <- model@presence
    bg <- model@background
    presence@data[variables[i]] <- NULL
    bg@data[variables[i]] <- NULL

    if (test) {
      test4jk <- model@test
      test4jk@data[variables[i]] <- NULL
    } else {
      test4jk <- NULL
    }

    jk_model <- trainMaxent(presence, bg, rm = model@rm, fc = model@fc,
                            type = model@type, test = test4jk, iter = model@iter)

    if (metric == "auc") {
      res[i, 2] <- jk_model@results["Training.AUC", ]
      if (test)
        res[i, 4] <- jk_model@results["Test.AUC", ]
    } else if (metric == "tss") {
      res[i, 2] <- tss(jk_model)
      if (test)
        res[i, 4] <- tss(jk_model, test4jk)
    } else {
      res[i, 2] <- aicc(jk_model, env, parallel)
    }
    models_without <- c(models_without, jk_model)
    pb$tick()

    if (with_only) {
      presence <- model@presence
      bg <- model@background
      presence@data <- presence@data[variables[i]]
      bg@data <- bg@data[variables[i]]

      if (test) {
        test4jk <- model@test
        test4jk@data <- model@test@data[variables[i]]
      } else {
        test4jk <- NULL
      }

      jk_model <- trainMaxent(presence, bg, rm = model@rm, fc = model@fc,
                              type = model@type, test = test4jk,
                              iter = model@iter)

      if (metric == "auc") {
        res[i, 3] <- jk_model@results["Training.AUC", ]
        if (test)
          res[i, 5] <- jk_model@results["Test.AUC", ]
      } else if (metric == "tss") {
        res[i, 3] <- tss(jk_model)
        if (test)
          res[i, 5] <- tss(jk_model, test4jk)
      } else {
        res[i, 3] <- aicc(jk_model, env, parallel)
      }

      models_withonly <- c(models_withonly, jk_model)
      pb$tick()
    }
  }

  jk_test <- as.data.frame(res)
  colnames(jk_test) <- labels
  jk_test[1] <- variables

  jk_test <- Filter(function(x) !all(is.na(x)), jk_test)

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

  gc()

  return(output)
}
