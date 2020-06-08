# Get presence locations from an SWD object
.get_presence <- function(swd) {
  return(swd@data[swd@pa == 1, , drop = FALSE])
}

# Get absence locations from an SWD object
.get_absence <- function(swd) {
  return(swd@data[swd@pa == 0, , drop = FALSE])
}

# Subset an SWD object using the fold partition
.subset_swd <- function(swd, fold) {

  data <- swd@data[fold, , drop = FALSE]
  coords <- swd@coords[fold, , drop = FALSE]
  rownames(data) <- NULL
  rownames(coords) <- NULL
  pa <- swd@pa[fold]

  output <- SWD(species = swd@species, data = data, coords = coords, pa = pa)

  return(output)
}

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

.get_footer <- function(model) {
  footer <- c()
  tuned_args <- .get_train_args(model)[getTunableArgs(model)]

  for (i in seq_along(tuned_args)) {
    footer <- c(footer, paste0(names(tuned_args)[i], ": ", tuned_args[[i]]))
  }

  return(paste(footer, collapse = "\n"))
}

.get_total_models <- function(pop, gen, remaining) {
  tot <- pop + (gen * remaining)
  return(tot)
}

.get_metric <- function(metric, model, test = NULL, env = NULL) {
  if (metric == "auc") {
    return(auc(model, test))
  } else if (metric == "tss") {
    return(tss(model, test))
  } else {
    return(aicc(model, env))
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

  tunable_hypers <- getTunableArgs(model)
  l <- length(tunable_hypers)
  labels <- c(tunable_hypers, .get_sdmtune_colnames(metric))

  res <- list()

  for (j in 1:l) {
    if (class(model) == "SDMmodel") {
      m <- model
    } else {
      m <- model@models[[1]]
    }
    res[[j]] <- slot(m@model, tunable_hypers[j])
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

  tunable_hypers <- getTunableArgs(models[[1]])
  l <- length(tunable_hypers)
  labels <- c(tunable_hypers, .get_sdmtune_colnames(metric))

  res <- matrix(nrow = length(models), ncol = length(labels))
  colnames(res) <- labels
  fcs <- vector("character", length = length(models))
  distrs <- vector("character", length = length(models))

  for (i in seq_along(models)) {
    if (class(models[[i]]) == "SDMmodel") {
      m <- models[[i]]
    } else {
      m <- models[[i]]@models[[1]]
    }
    for (j in 1:l) {
      if (tunable_hypers[j] == "distribution") {
        distrs[i] <- slot(m@model, tunable_hypers[j])
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

  if ("distribution" %in% tunable_hypers) {
    res$distribution <- distrs
  }
  if ("fc" %in% tunable_hypers) {
    res$fc <- fcs
  }

  output <- SDMtune(results = res, models = models)

  return(output)
}

.get_train_args <- function(model) {

  args <- list(data = model@data)

  if (class(model) == "SDMmodelCV") {
    args$folds <- model@folds
    model <- model@models[[1]]@model
  } else {
    args$folds <- NULL
    model <- model@model
  }

  args$method <- class(model)

  if (args$method == "Maxent") {
    args$fc <- model@fc
    args$reg <- model@reg
    args$iter <- model@iter
  } else if (args$method == "Maxnet") {
    args$fc <- model@fc
    args$reg <- model@reg
  } else if (args$method == "ANN") {
    args$size <- model@size
    args$decay <- model@decay
    args$rang <- model@rang
    args$maxit <- model@maxit
  } else if (args$method == "RF") {
    args$mtry <- model@mtry
    args$ntree <- model@ntree
    args$nodesize <- model@nodesize
  } else {
    args$distribution <- model@distribution
    args$n.trees <- model@n.trees
    args$interaction.depth <- model@interaction.depth
    args$shrinkage <- model@shrinkage
    args$bag.fraction <- model@bag.fraction
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
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Train a Maxnet model and get tunable hyperparameters
#' model <- train(method = "Maxnet", data = data, fc = "l")
#' get_tunable_args(model)
get_tunable_args <- function(model) {

  # TODO: remove this function in next release

  warning("This function will be deprecated in the next release, please use ",
          "\"getTunableArgs()\" instead.")

  if (class(model) == "SDMmodelCV") {
    method <- class(model@models[[1]]@model)
  } else {
    method <- class(model@model)
  }

  if (method == "Maxent") {
    args <- c("fc", "reg", "iter")
  } else if (method == "Maxnet") {
    args <- c("fc", "reg")
  } else if (method == "ANN") {
    args <- c("size", "decay", "rang", "maxit")
  } else if (method == "RF") (
    args <- c("mtry", "ntree", "nodesize")
  ) else {
    args <- c("distribution", "n.trees", "interaction.depth", "shrinkage",
              "bag.fraction")
  }

  return(args)
}

.create_model_from_settings <- function(model, settings, verbose = FALSE) {

  args <- .get_train_args(model)
  args[names(settings)] <- settings
  args$verbose <- verbose
  output <- suppressMessages(do.call("train", args))

  return(output)
}

.check_args <- function(model, metric, test = NULL, env = NULL, hypers = NULL) {
  # Throws exception if metric is aicc and env is not provided
  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide the 'env' argument if you want to use the AICc ",
         "metric!")
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
    # Throws exception if provided hypers are not tunable
    diff <- setdiff(names(hypers), getTunableArgs(model))
    if (length(diff) > 0)
      stop(paste(diff, "non included in tunable hyperparameters",
                 collapse = ", "))
  }
}

.get_hypers_grid <- function(model, hypers) {
  # Create data frame with all possible combinations of hyperparameters
  tunable_args <- .get_train_args(model)[getTunableArgs(model)]
  tunable_args[names(hypers)] <- hypers
  grid <- expand.grid(tunable_args, stringsAsFactors = FALSE)
  return(grid)
}

.args_name <- function(x) {
  output <- switch(x,
    "trainANN" = c("data", "size", "decay", "rang", "maxit"),
    "trainBRT" = c("data", "distribution", "n.trees", "interaction.depth",
                   "shrinkage", "bag.fraction"),
    "trainMaxent" = c("data", "reg", "fc", "iter"),
    "trainMaxnet" = c("data", "reg", "fc"),
    "trainRF" = c("data", "mtry", "ntree", "nodesize")
  )
  return(output)
}
