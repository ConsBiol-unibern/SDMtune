.get_model_class <- function(model) {
  if (class(model) == "SDMmodel") {
    return(class(model@model))
  } else {
    return(class(model@models[[1]]@model))
  }
}

.get_model_reg <- function(model) {
  if (class(model) == "SDMmodel") {
    return(model@model@reg)
  } else {
    return(model@models[[1]]@model@reg)
  }
}

.get_model_fc <- function(model) {
  if (class(model) == "SDMmodel") {
    return(model@model@fc)
  } else {
    return(model@models[[1]]@model@fc)
  }
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
    if (tunable_hypers[j] == "a") {
      res[[j]] <- nrow(model@a@data)
    } else {
      res[[j]] <- slot(model@model, tunable_hypers[j])
    }
  }
  res[[j + 1]] <- train_metric

  if (metric != "aicc") {
    res[[l + 2]] <- val_metric
    res[[l + 3]] <- res[[l + 1]] - res[[l + 2]]
  } else {
    res[[l + 2]] <- res[[l + 1]] - min(res[[l + 1]])
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
    for (j in 1:l) {
      if (tunable_hypers[j] == "a") {
        res[i, "a"] <- nrow(models[[i]]@a@data)
      } else if (tunable_hypers[j] == "fc") {
        fcs[i] <- models[[i]]@model@fc
      } else {
        res[i, tunable_hypers[j]] <- slot(models[[i]]@model, tunable_hypers[j])
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
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#'
#' @return character vector
#' @export
#'
#' @examples \dontrun{get_tunable_args(my_model)}
#'
#' @author Sergio Vignali
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
    args$a <- model@a
  }

  args$verbose = verbose

  output <- do.call("train", args)

  return(output)
}
