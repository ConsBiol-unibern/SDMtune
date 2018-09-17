#' Jackknife Test
#'
#' Run the Jackknife test for variable importance removing one variable at time.
#'
#' @param model SDMmodel object.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param variables vector. Variables used for the test, if not provided it
#' takes all the variables used to train the model, default is NULL.
#' @param test SWD. If provided it reports the result also for the test dataset.
#' Not used for **aicc**.
#' @param with_only logical. If TRUE it runs the test also for each variable in
#' isolation, default is TRUE.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param return_models logical, If TRUE returns all the models together with
#' the test result, default is FALSE.
#'
#' @details You need package \pkg{snow} to use parallel computation. Parallel
#' computation increases the speed only for big datasets due to the time
#' necessary to create the cluster.
#'
#' @return A data frame with the test results. If **return_model = TRUE** it
#' returns a list containing the test results together with the models.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples \dontrun{
#' doJk(model, variable = c('bio1', 'bio12'), with_only = TRUE)}
#'
#' @author Sergio Vignali
doJk <- function(model, metric = c("auc", "tss", "aicc"), variables = NULL,
                 test = NULL, with_only = TRUE, env = NULL, parallel = FALSE,
                 return_models = FALSE) {

  metric <- match.arg(metric)
  if (metric == "aicc" & is.null(env))
    stop("You must provide env parameter if you want to use AICc metric!")

  if (!is.null(test) & class(test) != "SWD")
    stop("test dataset must be a SWD object!")

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

  models_without <- models_withonly <- c()
  method <- class(model@model)

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

  if (method == "Maxent") {
    iter <- model@model@iter
    extra_args <- model@model@extra_args
  } else {
    iter <- NULL
    extra_args <- NULL
  }

  for (i in 1:length(variables)) {
    presence <- model@presence
    bg <- model@background
    presence@data[variables[i]] <- NULL
    bg@data[variables[i]] <- NULL

    jk_model <- train(method = method, presence = presence, bg = bg,
                      reg = model@model@reg, fc = model@model@fc, iter = iter,
                      extra_args = extra_args)

    if (metric == "auc") {
      res[i, 2] <- auc(jk_model)
      if (!is.null(test))
        res[i, 4] <- auc(jk_model, test)
    } else if (metric == "tss") {
      res[i, 2] <- tss(jk_model)
      if (!is.null(test))
        res[i, 4] <- tss(jk_model, test)
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

      jk_model <- train(method = method, presence = presence, bg = bg,
                        reg = model@model@reg, fc = model@model@fc, iter = iter,
                        extra_args = extra_args)

      if (metric == "auc") {
        res[i, 3] <- auc(jk_model)
        if (!is.null(test))
          res[i, 5] <- auc(jk_model, test)
      } else if (metric == "tss") {
        res[i, 3] <- tss(jk_model)
        if (!is.null(test))
          res[i, 5] <- tss(jk_model, test)
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
