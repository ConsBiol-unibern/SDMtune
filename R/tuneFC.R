#' Tune Feature Combination
#'
#' The function iterates among different Feature Classes combinations and for each of them
#' performs a run a MaxEnt model using the given regularization multiplier.
#'
#' @param model Maxent object.
#' @param fcs vector. A list of Feature Classes combination to be tested.
#' @param metric character. The metric used to evaluate the models, possible values are:
#' "auc", "tss" and "aicc", default is "auc".
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#'
#' @details You need package \pkg{snow} to use parallel computation and \pkg{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
#'
#' @return A \link{SDMsel} object.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' fc_test <- tuneFC(model, fcs = c("l", "lq", "lqp"), metric = "aicc",
#' env = predictors, parallel = T)}
#'
#' @author Sergio Vignali
tuneFC <- function(model, fcs, metric = c("auc", "tss", "aicc"), env = NULL,
                   parallel = FALSE) {

  if (nrow(model@test@data) == 0 & metric != "aicc")
    stop("You must first train the model using a test data set!")
  if (metric == "aicc" & is.null(env))
    stop("You must provide the env parameter if you want to use AICc metric!")

  pb <- progress::progress_bar$new(
    format = "Tune FC [:bar] :percent in :elapsedfull", total = length(fcs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  metric <- match.arg(metric)

  if (metric == "auc") {
    labels <- c("train_AUC", "test_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "test_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("it", "bg", "rm", "fc", labels)

  models <- list()
  res <- matrix(nrow = length(fcs), ncol = length(labels))

  if (nrow(model@test@data) == 0) {
    test <- NULL
  } else {
    test <- model@test
  }

  for (i in 1:length(fcs)) {

    if (fcs[i] == model@fc) {
      new_model <- model
    } else {
      new_model <- trainMaxent(model@presence, model@background, rm = model@rm,
                               fc = fcs[i], test = test, type = model@type,
                               iter = model@iter)
    }

    models <- c(models, new_model)
    if (metric == "auc") {
      res[i, 5] <- new_model@results["Training.AUC", ]
      res[i, 6] <- new_model@results["Test.AUC", ]
      res[i, 7] <- res[i, 5] - res[i, 6]
    } else if (metric == "tss") {
      res[i, 5] <- tss(new_model)
      res[i, 6] <- tss(new_model, new_model@test)
      res[i, 7] <- res[i, 5] - res[i, 6]
    } else {
      res[i, 5] <- aicc(new_model, env, parallel)
    }

    pb$tick(1)
  }

  res[, 1] <- model@iter
  res[, 2] <- nrow(model@background@data)
  res[, 3] <- model@rm

  if (metric == "aicc") {
    res[, 6] <- round(res[, 5] - min(res[, 5]), 4)
  } else {
    res[, 7] <- round(res[, 5] - res[, 6], 4)
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- fcs

  output <- SDMsel(results = res, models = models)

  gc()

  return(output)
}
