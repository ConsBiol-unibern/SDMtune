#' Tune Feature Combination
#'
#' The function iterates among different Feature Classes combinations and for
#' each of them performs trains a different MaxEnt model.
#'
#' @param model SDMmodel object.
#' @param fcs vector with all the Feature Classes combination to be tested.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc,
#' default is NULL.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#'
#' @details You need package \pkg{snow} to use parallel computation and
#' \pkg{rgdal} to save the prediction in a raster file. Parallel computation
#' increases the speed only for big datasets due to the time necessary to create
#' the cluster.
#'
#' @family tuning functions
#'
#' @return A \link{SDMtune} object.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' fc_test <- tuneFC(model, fcs = c("l", "lq", "lqp"), metric = "aicc",
#' env = predictors, parallel = T)}
#'
#' @author Sergio Vignali
tuneFC <- function(model, fcs, metric = c("auc", "tss", "aicc"), test = NULL,
                   env = NULL, parallel = FALSE) {

  metric <- match.arg(metric)

  if (is.null(test) & metric != "aicc")
    stop("You need to provide a test dataset!")
  if (metric == "aicc" & is.null(env))
    stop("You must provide the env parameter if you want to use AICc metric!")

  pb <- progress::progress_bar$new(
    format = "Tune FC [:bar] :percent in :elapsedfull", total = length(fcs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  method <- class(model@model)

  if (method == "Maxent") {
    iter <- model@model@iter
    extra_args <- model@model@extra_args
  } else {
    iter <- NULL
    extra_args <- NULL
  }

  if (metric == "auc") {
    labels <- c("train_AUC", "test_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "test_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("bg", "rm", "fc", labels)

  models <- list()
  res <- matrix(nrow = length(fcs), ncol = length(labels))

  for (i in 1:length(fcs)) {

    if (fcs[i] == model@model@fc) {
      new_model <- model
    } else {
      new_model <- train(method = method, presence = model@presence,
                         bg = model@background, rm = model@model@rm,
                         fc = fcs[i], iter = iter, extra_args = extra_args)
    }

    models <- c(models, new_model)
    if (metric == "auc") {
      res[i, 4] <- auc(new_model)
      res[i, 5] <- auc(new_model, test)
    } else if (metric == "tss") {
      res[i, 4] <- tss(new_model)
      res[i, 5] <- tss(new_model, test)
    } else {
      res[i, 4] <- aicc(new_model, env, parallel)
    }

    pb$tick(1)
  }

  res[, 1] <- nrow(model@background@data)
  res[, 2] <- model@model@rm

  if (metric == "aicc") {
    res[, 5] <- round(res[, 4] - min(res[, 4]), 4)
  } else {
    res[, 6] <- round(res[, 4] - res[, 5], 4)
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- fcs

  output <- SDMtune(results = res, models = models)
  gc()

  return(output)
}
