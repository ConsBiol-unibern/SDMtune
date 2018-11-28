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
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc,
#' default is NULL.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
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

  if (is.null(test) & metric != "aicc")
    stop("You need to provide a test dataset!")
  if (metric == "aicc" & is.null(env))
    stop("You must provide the env parameter if you want to use AICc metric!")

  if (!is.null(seed))
    set.seed(seed)

  pb <- progress::progress_bar$new(
    format = "Tune Bg [:bar] :percent in :elapsedfull", total = length(bgs),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  method <- class(model@model)

  if (metric == "auc") {
    labels <- c("train_AUC", "test_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "test_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("bg", "reg", "fc", labels)

  models <- list()
  res <- matrix(nrow = length(bgs), ncol = length(labels))

  vars <- colnames(model@presence@data)
  bg4test@data <- bg4test@data[vars]

  folds <- sample(nrow(bg4test@data))

  for (i in 1:length(bgs)) {

    if (bgs[i] == nrow(model@background@data)) {
      new_model <- model
    } else {
      bg <- bg4test
      bg@data <- bg4test@data[folds[1:bgs[i]], ]
      bg@coords <- bg4test@coords[folds[1:bgs[i]], ]
      if (method == "Maxent") {
        new_model <- train(method = method, presence = model@presence,
                           bg = bg, reg = model@model@reg, fc = model@model@fc,
                           iter = model@model@iter,
                           extra_args = model@model@extra_args)
      } else {
        new_model <- train(method = method, presence = model@presence,
                           bg = bg, reg = model@model@reg, fc = model@model@fc)
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

  res[, 1] <- bgs
  res[, 2] <- model@model@reg

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
