.get_model_class <- function(model) {
  if (class(model) == "SDMmodelCV") {
    model <- model@models[[1]]
  }
  return(class(model@model))
}

.get_model_reg <- function(model) {
  if (class(model) == "SDMmodelCV") {
    model <- model@models[[1]]
  }
  return(model@model@reg)
}

.get_model_fc <- function(model) {
  if (class(model) == "SDMmodelCV") {
    model <- model@models[[1]]
  }
  return(model@model@fc)
}

.get_model_hyperparams <- function(model) {
  if (class(model) == "SDMmodelCV")
    model <- model@models[[1]]
  return(paste("Reg:", model@model@reg, "FC:", model@model@fc,
               "#Bg:", nrow(model@a@data)))
}

.get_footer <- function(model) {
  footer <- c()
  tuned_args <- .get_train_args(model)[get_tunable_args(model)]

  for (i in 1:length(tuned_args)) {
    if (names(tuned_args)[i] != "a") {
      footer <- c(footer, paste0(names(tuned_args)[i], ": ",
                                 tuned_args[[i]]))
    } else {
      footer <- c(footer, paste0("a", ": ", nrow(tuned_args[[i]]@data)))
    }
  }
  return(paste(footer, collapse = "\n"))
}

.get_total_models <- function(pop, gen, remaining) {
  tot <- pop + (gen * remaining)
  return(tot)
}

.get_metric <- function(metric, model, test = NULL, env = NULL,
                        parallel = FALSE) {
  if (metric == "auc") {
    return(auc(model, test))
  } else if (metric == "tss") {
    return(tss(model, test))
  } else {
    return(aicc(model, env, parallel))
  }
}

.get_metric_label <- function(metric) {
  if (metric == "auc") {
    return("AUC")
  } else if (metric == "tss") {
    return("TSS")
  } else {
    return("AICc")
  }
}

.get_sdmtune_colnames <- function(metric) {
  if (metric == "auc") {
    return(c("train_AUC", "test_AUC", "diff_AUC"))
  } else if (metric == "tss") {
    return(c("train_TSS", "test_TSS", "diff_TSS"))
  } else {
    return(c("AICc", "delta_AICc"))
  }
}

.create_sdmtune_result <- function(model, metric, train_metric, val_metric) {

  tunable_hypers <- get_tunable_args(model)
  l <- length(tunable_hypers)
  labels <- c(tunable_hypers, .get_sdmtune_colnames(metric))

  res <- list()

  for (j in 1:l) {
    if (class(model) == "SDMmodel") {
      m <- model
    } else {
      m <- model@models[[1]]
    }
    if (tunable_hypers[j] == "a") {
      res[[j]] <- nrow(m@a@data)
    } else {
      res[[j]] <- slot(m@model, tunable_hypers[j])
    }
  }
  res[[j + 1]] <- train_metric

  if (metric != "aicc") {
    res[[l + 2]] <- val_metric
    res[[l + 3]] <- res[[l + 1]] - res[[l + 2]]
  } else {
    res[[l + 2]] <- NA
  }

  names(res) <- labels

  return(res)
}

.create_sdmtune_output <- function(models, metric, train_metric, val_metric) {

  tunable_hypers <- get_tunable_args(models[[1]])
  l <- length(tunable_hypers)
  labels <- c(tunable_hypers, .get_sdmtune_colnames(metric))

  res <- matrix(nrow = length(models), ncol = length(labels))
  colnames(res) <- labels
  fcs <- vector("character", length = length(models))

  for (i in 1:length(models)) {
    if (class(models[[i]]) == "SDMmodel") {
      m <- models[[i]]
    } else {
      m <- models[[i]]@models[[1]]
    }
    for (j in 1:l) {
      if (tunable_hypers[j] == "a") {
        res[i, "a"] <- nrow(m@a@data)
      } else if (tunable_hypers[j] == "fc") {
        fcs[i] <- m@model@fc
      } else {
        res[i, tunable_hypers[j]] <- slot(m@model, tunable_hypers[j])
      }
    }
    res[i, l + 1] <- train_metric[i, 2]
    if (metric != "aicc")
      res[i, l + 2] <- val_metric[i, 2]
  }

  if (metric != "aicc") {
    res[, l + 3] <- res[, l + 1] - res[, l + 2]
  } else {
    res[, l + 2] <- res[, l + 1] - min(res[, l + 1])
  }

  res <- as.data.frame(res, stringsAsFactors = FALSE)

  if ("fc" %in% tunable_hypers)
    res$fc <- fcs

  output <- SDMtune(results = res, models = models)

  return(output)
}

.get_train_args <- function(model) {

  args <- list(p = model@p, a = model@a)

  if (class(model) == "SDMmodelCV") {
    args$rep <- length(model@models)
    args$folds <- model@folds
    model <- model@models[[1]]@model
  } else {
    args$rep <- 1
    args$folds <- NULL
    model <- model@model
  }

  args$method <- class(model)
  args$fc <- model@fc
  args$reg <- model@reg

  if (args$method == "Maxent") {
    args$iter <- model@iter
    args$extra_args <- model@extra_args
  }
  return(args)
}

#' Get Tunable Arguments
#'
#' Returns the name of all function arguments that can be tuned for a given
#' model.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#'
#' @return character vector
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
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
#' # Train a Maxent model and get tunable hyperparameters
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "l")
#' get_tunable_args(model)
#'
#' \donttest{
#' # Train a Maxnet model and get tunable hyperparameters
#' model <- train(method = "Maxent", p = presence, a = bg, fc = "l")
#' get_tunable_args(model)
#' }
get_tunable_args <- function(model) {

  if (class(model) == "SDMmodelCV") {
    method <- class(model@models[[1]]@model)
  } else {
    method <- class(model@model)
  }

  if (method == "Maxent") {
    args <- c("a", "fc", "reg", "iter")
  } else {
    args <- c("a", "fc", "reg")
  }

  return(args)
}

.create_model_from_settings <- function(model, settings, bg4test = NULL,
                                        bg_folds = NULL, verbose = FALSE) {

  args <- .get_train_args(model)
  args[names(settings)] <- settings
  if (!is.null(bg_folds)) {
    bg <- bg4test
    bg@data <- bg4test@data[bg_folds[1:settings$a], ]
    bg@coords <- bg4test@coords[bg_folds[1:settings$a], ]
    row.names(bg@data) <- NULL
    row.names(bg@coords) <- NULL
    args$a <- bg
  } else if ("a" %in% names(settings) & class(settings$a) != "SWD") {
    if (settings$a != nrow(model@a@data))
      warning("Ignored number of 'a' in settings!")
    args$a <- model@a
  }

  args$verbose <- verbose

  output <- suppressMessages(do.call("train", args))

  return(output)
}

.check_args <- function(model, metric, test = NULL, bg4test = NULL, env = NULL,
                        hypers = NULL) {
  # Throws exception if metric is aicc and env is not provided
  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if model is SDMmodel, metric is not aicc and
  # test is not provided
  if (class(model) == "SDMmodel" & is.null(test) & metric != "aicc") {
    stop("You need to provide a test dataset!")
  }
  # Throws exception if metric is aicc and model is SDMmodelCV
  if (class(model) == "SDMmodelCV" & metric == "aicc")
    stop("Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Check hypers
  if (!is.null(hypers)) {
    # Throws exception if hypers includes 'a' and bg4test is not provided
    if (!is.null(hypers$a) & is.null(bg4test))
      stop("bg4test must be provided to tune background locations!")
    # Throws exception if max hypers 'a' > than nrow bg4test
    if (!is.null(hypers$a)) {
      if (max(hypers$a) > nrow(bg4test@data))
        stop(paste0("Maximum number of 'a' hyperparameter cannot be more than ",
                    nrow(bg4test@data), "!"))
    }
    # Throws exception if provided hypers are not tunable
    diff <- setdiff(names(hypers), get_tunable_args(model))
    if (length(diff) > 0)
      stop(paste(diff, "non included in tunable hyperparameters",
                 collapse = ", "))
  }
}

.get_hypers_grid <- function(model, hypers) {
  # Create data frame with all possible combinations of hyperparameters
  tunable_args <- .get_train_args(model)[get_tunable_args(model)]
  tunable_args[names(hypers)] <- hypers
  if (is.null(hypers$a))
    tunable_args$a <- nrow(model@a@data)
  grid <- expand.grid(tunable_args, stringsAsFactors = FALSE)
  return(grid)
}
