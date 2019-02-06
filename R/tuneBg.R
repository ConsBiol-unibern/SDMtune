#' Tune number of Background locations
#'
#' Test different sizes of background locations.
#'
#' @param model SDMmodel object.
#' @param bg4test SWD. The dataset with the maximum number of background
#' locations to be tested given as MaxentSWD object.
#' @param bgs vector. A sequence of number of background locations to test. The
#' maximum number must be lower than the maximum number of background locations
#' in bg4test.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc
#' and SDMmodelCV objects, default is NULL.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param seed integer. The value used to set the seed in order to have
#' consistent results, default is NULL.
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
#' @importFrom jsonlite toJSON
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' bg_test <- tuneBg(model, bg4test,bgs = seq(5000, 30000, 5000),
#' metric = "auc", test = test, seed = 25)}
#'
#' @author Sergio Vignali
tuneBg <- function(model, bg4test, bgs, metric = c("auc", "tss", "aicc"),
                   test = NULL, env = NULL, parallel = FALSE, seed = NULL) {

  metric <- match.arg(metric)

  if (max(bgs) > nrow(bg4test@data))
    stop(paste("Maximum number of bgs cannot be more than!",
               nrow(bg4test@data)))

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
    format = "Tune Bg [:bar] :percent in :elapsedfull", total = length(bgs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

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
  res <- matrix(nrow = length(bgs), ncol = length(labels))

  vars <- colnames(model@presence@data)
  bg4test@data <- bg4test@data[vars]

  folds_bg <- sample(nrow(bg4test@data))

  # Create chart
  settings <- list(metric = .get_metric_label(metric),
                   title = "Tune Backgrounds",
                   x_label = "backgrounds",
                   min = min(bgs),
                   max = max(bgs),
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
  line_footer <- vector("character", length = length(bgs))

  for (i in 1:length(bgs)) {

    if (bgs[i] == nrow(model@background@data)) {
      new_model <- model
    } else {
      bg <- bg4test
      bg@data <- bg4test@data[folds_bg[1:bgs[i]], ]
      bg@coords <- bg4test@coords[folds_bg[1:bgs[i]], ]
      # Reset row names
      rownames(bg@data) <- NULL
      rownames(bg@coords) <- NULL
      if (method == "Maxent") {
        new_model <- train(method = method, presence = model@presence, bg = bg,
                           reg = object@model@reg, fc = object@model@fc,
                           replicates = rep, verbose = FALSE, folds = folds,
                           iter = object@model@iter,
                           extra_args = object@model@extra_args)
      } else {
        new_model <- train(method = method, presence = model@presence, bg = bg,
                           reg = object@model@reg, fc = object@model@fc,
                           replicates = rep, verbose = FALSE, folds = folds)
      }
    }

    models <- c(models, new_model)
    res[i, 4] <- .get_metric(metric, new_model, env = env, parallel = parallel)
    train_metric[i, ] <- list(bgs[i], res[i, 4])
    if (metric != "aicc") {
      res[i, 5] <- .get_metric(metric, new_model, test = test)
      val_metric[i, ] <- list(bgs[i], res[i, 5])
    }
    line_footer[i] <- .get_model_hyperparams(new_model)

    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      lineFooter = line_footer, stop = FALSE))
    Sys.sleep(.1)

    pb$tick(1)
  }

  .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                    lineFooter = line_footer, stop = TRUE))

  res[, 1] <- bgs
  res[, 2] <- object@model@reg

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
