#' Tune number of Background locations
#'
#' Test different sizes of background locations.
#'
#' @param model Maxent object.
#' @param bg4test SWD. The dataset with the maximum number of background locations to be tested given as MaxentSWD object.
#' @param bgs vector. A sequence of number of background locations to test. The maximum number must be lower thatn the maximum number of
#' background locations in bg4test. Default is the maximum number of background locations in bg4test.
#' @param metric character. The metric used to evaluate the models, possible values are:
#' "auc", "tss" and "aicc", default is "auc"
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#' @param extra_args vector Extra arguments used to run MaxEnt..
#' @param seed integer. The value used to set the seed in order to have consistent results, default is NULL.
#'
#' @return A \link{SDMsel} object.
#' @export
#'
#' @examples
#' \dontrun{
#' test_bg <- tuneBg(model, bg4test,bgs = seq(5000, 30000, 5000), metric = "auc", seed = 25)}
#'
#' @author Sergio Vignali
tuneBg <- function(model, bg4test, bgs, env = NULL,
                   metric = c("auc", "tss", "aicc"), parallel = FALSE,
                   extra_args = NULL, seed = NULL) {

  if (max(bgs) > nrow(bg4test@data))
    stop(paste("Maximum number of bgs cannot be more than!", nrow(bg4test@data)))

  if (nrow(model@test@data) == 0 & metric != "aicc")
    stop("You must first train the model using a test data set!")

  if (!is.null(seed)) set.seed(seed)

  pb <- progress::progress_bar$new(
    format = "Tune Bg [:bar] :percent in :elapsedfull", total = length(bgs),
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
  res <- matrix(nrow = length(bgs), ncol = length(labels))

  folds <- sample(nrow(bg4test@data))
  variables <- colnames(train@data)
  bg4test@data <- bg4test@data[variables]

  for (i in 1:length(bgs)) {

    if (bgs[i] == nrow(model@background@data)) {
      new_model <- model
    } else {
      bg <- bg4test
      bg@data <- bg4test@data[folds[1:bgs[i]], ]
      new_model <- trainMaxent(model@presence, bg, rm = model@rm, fc = model@fc,
                               test = model@test, type = model@type,
                               extra_args = extra_args)
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

  res[, 1] <- model@iterations
  res[, 2] <- bgs
  res[, 3] <- model@rm

  if (metric == "aicc") {
    res[, 6] <- round(res[, 5] - min(res[, 5]), 4)
  } else {
    res[, 7] <- round(res[, 5] - res[, 6], 4)
  }
  res[, 4] <- model@fc
  res <- as.data.frame(res)
  colnames(res) <- labels

  output <- SDMsel(results = res, models = models)

  return(output)
}
