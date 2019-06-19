#' Jackknife Test
#'
#' Run the Jackknife test for variable importance removing one variable at time.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param variables vector. Variables used for the test, if not provided it
#' takes all the variables used to train the model, default is \code{NULL}.
#' @param test \linkS4class{SWD}. If provided it reports the result also for the
#' test dataset. Not used for **aicc** and \linkS4class{SDMmodelCV}.
#' @param with_only logical. If \code{TRUE} it runs the test also for each
#' variable in isolation, default is \code{TRUE}.
#' @param env \code{\link[raster]{stack}} containing the environmental
#' variables, used only with "aicc", default is \code{NULL}.
#' @param parallel logical, if \code{TRUE} it uses parallel computation, default
#' is \code{FALSE}. Used only with AICc.
#' @param return_models logical, if \code{TRUE} returns all the models together
#' with the test result, default is \code{FALSE}.
#'
#' @details Parallel computation increases the speed only for large datasets due
#' to the time necessary to create the cluster.
#'
#' @return A data frame with the test results. If "\code{return_model = TRUE}"
#' it returns a list containing the test results together with the models.
#' @export
#' @importFrom progress progress_bar
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' bg <- prepareSWD(species = "Vultur gryphus", coords = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "lq")
#'
#' # Execute the Jackknife test only for the environmental variables "bio1" and
#' # "bio12", using the metric AUC
#' doJk(model, metric = "auc", variables = c("bio1", "bio12"), test = test)
#'
#' # Execute the Jackknife test only for the environmental variables "bio1" and
#' # "bio12", using the metric TSS but without running the test for one single
#' # variable
#' doJk(model, metric = "tss", variables = c("bio1", "bio12"), test = test,
#'      with_only = FALSE)
#'
#' # Execute the Jackknife test only for the environmental variables "bio1" and
#' # "bio12", using the metric AICc but without running the test for one single
#' # variable
#' doJk(model, metric = "aicc", variables = c("bio1", "bio12"),
#'      with_only = FALSE, env = predictors)
#'
#' # Execute the Jackknife test for all the environmental variables using the
#' # metric AUC and returning all the trained models
#' jk <- doJk(model, metric = "auc", test = test, return_models = TRUE)
#' jk$results
#' jk$models_without
#' jk$models_withonly
#' }
doJk <- function(model, metric, variables = NULL, test = NULL, with_only = TRUE,
                 env = NULL, parallel = FALSE, return_models = FALSE) {

  metric <- match.arg(metric, c("auc", "tss", "aicc"))

  .check_args(model, metric = metric, test = test, env = env)

  if (is.null(variables))
    variables <- colnames(model@p@data)

  n <- length(variables)

  if (with_only) {
    tot <- n * 2
  } else {
    tot <- n
  }

  pb <- progress::progress_bar$new(
    format = "Jk Test [:bar] :percent in :elapsedfull", total = tot,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  models_without <- vector("list", length = n)
  models_withonly <- vector("list", length = n)

  res <- matrix(nrow = n, ncol = 5)
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

  for (i in 1:n) {
    p <- old_model@p
    a <- old_model@a
    p@data[variables[i]] <- NULL
    a@data[variables[i]] <- NULL

    if (metric != "aicc" & class(model) != "SDMmodelCV") {
      t <- test
      t@data[variables[i]] <- NULL
    }

    settings <- list("p" = p, "a" = a)

    jk_model <- .create_model_from_settings(model, settings)

    res[i, 2] <- .get_metric(metric, jk_model, env = env, parallel = parallel)
    if (metric != "aicc")
      res[i, 4] <- .get_metric(metric, jk_model, test = t)

        models_without[[i]] <- jk_model
    pb$tick()

    if (with_only) {
      p <- old_model@p
      a <- old_model@a
      p@data <- p@data[variables[i]]
      a@data <- a@data[variables[i]]

      if (metric != "aicc" & class(model) != "SDMmodelCV") {
        t <- test
        t@data <- t@data[variables[i]]
      }

      settings <- list("p" = p, "a" = a)

      jk_model <- .create_model_from_settings(model, settings)

      res[i, 3] <- .get_metric(metric, jk_model, env = env, parallel = parallel)
      if (metric != "aicc")
        res[i, 5] <- .get_metric(metric, jk_model, test = t)

      models_withonly[[i]] <- jk_model
      pb$tick()
    }
  }

  jk_test <- as.data.frame(res)
  colnames(jk_test) <- labels
  jk_test[1] <- variables

  jk_test <- Filter(function(x) !all(is.na(x)), jk_test)

  if (return_models) {
    if (with_only) {
      output <- list(results = jk_test, models_without =  models_without,
                     models_withonly = models_withonly)
    } else {
      output <- list(results = jk_test, models_without = models_without)
    }

  } else {
    output <- jk_test
  }

  return(output)
}
