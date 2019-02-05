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
#' and SDMmodelCV objects, default is NULL.
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

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  pb <- progress::progress_bar$new(
    format = "Tune Reg [:bar] :percent in :elapsedfull", total = length(regs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (class(model) == "SDMmodel") {
    rep <- 1
    method <- class(model@model)
    folds <- NULL
    object <- model
  } else {
    rep <- length(model@models)
    method <- class(model@models[[1]]@model)
    folds <- model@folds
    object <- model@models[[1]]
    test <- TRUE
  }

  labels <- .get_tune_labels(metric)

  models <- list()
  res <- matrix(nrow = length(regs), ncol = length(labels))

  # Create chart
  settings <- list(metric = .get_metric_label(metric),
                   title = "Tune Regularization",
                   x_label = "regularization multiplier",
                   min = min(regs),
                   max = max(regs),
                   labels = c(""),
                   show_line = TRUE,
                   update = TRUE)

  data = list()

  folder <- tempfile("SDMtune")

  .create_chart(folder = folder, script = "tuneModel.js",
                settings = settings, data = data)

  # metric used for chart
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)
  line_footer <- vector("character", length = length(regs))

  for (i in 1:length(regs)) {

    if (regs[i] == object@model@reg) {
      new_model <- model
    } else {
      if (method == "Maxent") {
        new_model <- train(method = method, presence = model@presence,
                           bg = model@background, reg = regs[i],
                           fc = object@model@fc, replicates = rep,
                           verbose = FALSE, folds = folds,
                           iter = object@model@iter,
                           extra_args = object@model@extra_args)
      } else {
        new_model <- train(method = method, presence = model@presence,
                           bg = model@background, reg = regs[i],
                           fc = object@model@fc, replicates = rep,
                           verbose = FALSE, folds = folds)
      }
    }

    models <- c(models, new_model)
    res[i, 4] <- .get_metric(metric, new_model, env = env, parallel = parallel)
    train_metric[i, ] <- list(regs[i], res[i, 4])
    if (metric != "aicc") {
      res[i, 5] <- .get_metric(metric, new_model, test = test)
      val_metric[i, ] <- list(regs[i], res[i, 5])
    }
    line_footer[i] <- .get_model_hyperparams(new_model)

    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      lineFooter = line_footer, stop = FALSE))
    Sys.sleep(.1)

    pb$tick(1)
  }

  .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                    lineFooter = line_footer, stop = TRUE))

  res[, 1] <- nrow(model@background@data)
  res[, 2] <- regs

  if (metric == "aicc") {
    res[, 5] <- round(res[, 4] - min(res[, 4]), 4)
  } else {
    res[, 6] <- round(res[, 4] - res[, 5], 4)
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- object@model@fc

  output <- SDMtune(results = res, models = models)

  return(output)
}
