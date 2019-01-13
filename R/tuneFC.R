#' Tune Feature Combination
#'
#' The function iterates among different Feature Classes combinations and for
#' each of them performs trains a different MaxEnt model.
#'
#' @param model SDMmodel or SWDmodelCV object.
#' @param fcs vector with all the Feature Classes combination to be tested.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc
#' and SDMmodelCV objects, default is NULL.
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
#' @importFrom jsonlite toJSON
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
    format = "Tune FC [:bar] :percent in :elapsedfull", total = length(fcs),
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
  res <- matrix(nrow = length(fcs), ncol = length(labels))

  # Create chart
  context <- list(tot_models = length(fcs),
                  metric = .get_metric_label(metric),
                  title = "Tune Feature Combinations",
                  x_label = "feature combination",
                  labels = jsonlite::toJSON(fcs))

  folder <- .create_chart(template = "tuneTemplate", context = context)

  # metric used for chart
  train_metric <- rep(NA_real_, length(fcs))
  val_metric <- rep(NA_real_, length(fcs))
  line_footer <- vector("character", length = length(fcs))

  for (i in 1:length(fcs)) {

    if (fcs[i] == object@model@fc) {
      new_model <- model
    } else {
      if (method == "Maxent") {
        new_model <- train(method = method, presence = model@presence,
                           bg = model@background, reg = object@model@reg,
                           fc = fcs[i], replicates = rep, verbose = FALSE,
                           folds = folds, iter = object@model@iter,
                           extra_args = object@model@extra_args)
      } else {
        new_model <- train(method = method, presence = model@presence,
                           bg = model@background, reg = object@model@reg,
                           fc = fcs[i], replicates = rep, verbose = FALSE,
                           folds = folds)
      }
    }

    models <- c(models, new_model)
    res[i, 4] <- .get_metric(metric, new_model, env = env, parallel = parallel)
    train_metric[i] <- res[i, 4]
    if (metric != "aicc") {
      res[i, 5] <- .get_metric(metric, new_model, test = test)
      val_metric[i] <- res[i, 5]
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
  res[, 2] <- object@model@reg

  if (metric == "aicc") {
    res[, 5] <- round(res[, 4] - min(res[, 4]), 4)
  } else {
    res[, 6] <- round(res[, 4] - res[, 5], 4)
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- fcs

  output <- SDMtune(results = res, models = models)

  return(output)
}
