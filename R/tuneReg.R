#' Tune Regularization
#'
#' Given a sequence of regularization multipliers, the function runs several
#' models increasing the regularization multiplier.
#'
#' @param model SDMmodel or SWDmodelCV object.
#' @param regs vector. A sequence of regularization multipliers to test.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc
#' or SDMmodelCV objects, default is NULL.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE.
#'
#' @details You need package \pkg{snow} to use parallel computation and
#' \pkg{rgdal} to save the prediction in a raster file. Parallel computation
#' increases the speed only for big datasets due to the time necessary to create
#' the cluster. The minimum value of reg allow is for Maxent models is 0.001, if
#' lower MaxEnt crasches; for Maxnet model is 0.1, if lower doesn't converg.
#'
#' @family tuning functions
#'
#' @return A \link{SDMtune} object.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' reg_test <- tuneREG(model, metric = "aicc", env = predictors, parallel = T)}
#'
#' @author Sergio Vignali
tuneReg <- function(model, regs, metric = c("auc", "tss", "aicc"), test = NULL,
                   env = NULL, parallel = FALSE) {

  metric <- match.arg(metric)

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  }

  if (metric == "aicc" & is.null(env))
    stop("You must provide the env argument if you want to use AICc metric!")

  pb <- progress::progress_bar$new(
    format = "Tune Reg [:bar] :percent in :elapsedfull", total = length(regs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  presence <- model@presence
  old_model <- model
  if (class(model) == "SDMmodel") {
    rep <- 1
    method <- class(model@model)
    bg <- model@background
    folds <- NULL
  } else {
    if (!is.logical(test))
      rep <- length(model@models)
    method <- class(model@models[[1]]@model)
    bg <- model@models[[1]]@background
    folds <- model@folds
    model <- model@models[[1]]
    test = TRUE
  }

  if (metric == "auc") {
    labels <- c("train_AUC", "test_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "test_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("bg", "reg", "fc", labels)

  models <- list()
  res <- matrix(nrow = length(regs), ncol = length(labels))

  for (i in 1:length(regs)) {

    if (regs[i] == model@model@reg) {
      new_model <- old_model
    } else {
      if (method == "Maxent") {
        new_model <- train(method = method, presence = presence, bg = bg,
                           reg = regs[i], fc = model@model@fc, replicates = rep,
                           verbose = FALSE, folds = folds,
                           iter = model@model@iter,
                           extra_args = model@model@extra_args)
      } else {
        new_model <- train(method = method, presence = presence, bg = bg,
                           reg = regs[i], fc = model@model@fc, replicates = rep,
                           verbose = FALSE, folds = folds)
      }
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

  res[, 1] <- nrow(bg@data)
  res[, 2] <- regs

  if (metric == "aicc") {
    res[, 5] <- round(res[, 4] - min(res[, 4]), 4)
  } else {
    res[, 6] <- round(res[, 4] - res[, 5], 4)
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- model@model@fc

  output <- SDMtune(results = res, models = models)
  gc()

  return(output)
}
