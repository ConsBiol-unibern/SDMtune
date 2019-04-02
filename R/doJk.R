#' Jackknife Test
#'
#' Run the Jackknife test for variable importance removing one variable at time.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param variables vector. Variables used for the test, if not provided it
#' takes all the variables used to train the model, default is NULL.
#' @param test \link{SWD}. If provided it reports the result also for the test
#' dataset. Not used for **aicc** and \link{SDMmodelCV}.
#' @param with_only logical. If TRUE it runs the test also for each variable in
#' isolation, default is TRUE.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
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
doJk <- function(model, metric, variables = NULL,
                 test = NULL, with_only = TRUE, env = NULL, parallel = FALSE,
                 return_models = FALSE) {

  metric <- match.arg(metric, c("auc", "tss", "aicc"))

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  if (is.null(variables))
    variables <- colnames(model@p@data)

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

  old_model <- model

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  for (i in 1:length(variables)) {
    p <- old_model@p
    a <- old_model@a
    p@data[variables[i]] <- NULL
    a@data[variables[i]] <- NULL
    settings <- list("p" = p, "a" = a)

    jk_model <- .create_model_from_settings(model, settings)

    res[i, 2] <- .get_metric(metric, jk_model, env = env, parallel = parallel)
    if (metric != "aicc")
      res[i, 4] <- .get_metric(metric, jk_model, test = test)

        models_without <- c(models_without, jk_model)
    pb$tick()

    if (with_only) {
      p <- old_model@p
      a <- old_model@a
      p@data <- p@data[variables[i]]
      a@data <- a@data[variables[i]]
      settings <- list("p" = p, "a" = a)

      jk_model <- .create_model_from_settings(model, settings)

      res[i, 3] <- .get_metric(metric, jk_model, env = env, parallel = parallel)
      if (metric != "aicc")
        res[i, 5] <- .get_metric(metric, jk_model, test = test)

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

  return(output)
}
