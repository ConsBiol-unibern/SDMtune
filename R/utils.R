get_metric <- function(metric, model, test = NULL, env = NULL,
                       parallel = FALSE) {
  if (metric == "auc") {
    return(auc(model, test))
  } else if (metric == "tss") {
    return(tss(model, test))
  } else {
    return(aicc(model, env, parallel))
  }
}

get_metric_label <- function(metric) {
  if (metric == "auc") {
    return("AUC")
  } else if (metric == "tss") {
    return("TSS")
  } else {
    return("AICc")
  }
}

get_begin_text <- function(n, text = "") {
  if (n == 0) {
    if (text == "") {
      return("Starting model - ")
    } else {
      return("Random population, best model - ")
    }
  } else {
    return(paste("Generation", n, ", best model - "))
  }
}

get_metric_text <- function(n, metric, train_metric, val_metric = NULL,
                            text = "") {
  begin_text <- get_begin_text(n, text)
  metric_label <- get_metric_label(metric)
  text <- paste0(begin_text, "Training ", metric_label, ": ", train_metric)
  if (!is.null(val_metric))
    text <- paste0(text, " | Validation ", metric_label, ": ", val_metric)

  return(text)
}

get_total_models <- function(pop, gen, remaining) {
  tot <- pop + (gen * remaining)

  return(tot)
}

get_rank_index <- function(metric, metrics) {
  if (metric == "aicc") {
    # The best model is the one with the lowest AICc
    index <- order(metrics[[1]])
  } else {
    # The best model is the one with the highest AUC or TSS
    diff_metric <- metrics[[1]] - metrics[[2]]
    # Check if the models are all overfitting the validation dataset
    if (!any(diff_metric > 0))
      return(FALSE)
    # Ordered index of dereasing validation metric
    o <- order(-metrics[[2]])
    # Good models are those not overfitting the validation dataset
    good_models <- o[o %in% which(diff_metric > 0)]
    # Bad models have diff_metric >= 0
    bad_models <- o[!o %in% good_models]
    # Ordered index of decreasomg diff_metric
    odm <- order(-diff_metric)
    # Ordered index of bad_models from the one less overfitting
    bad_models <- odm[odm %in% bad_models]
    # Combine indexes
    index <- c(good_models, bad_models)
  }
  return(index)
}

create_optimise_output <- function(models, metric, metrics) {
  if (metric == "auc") {
    labels <- c("train_AUC", "val_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "val_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("bg", "reg", "fc", labels)

  res <- matrix(nrow = length(models), ncol = length(labels))
  fcs <- vector("character", length = length(models))

  for (i in 1:length(models)) {
    res[i, 1] <- nrow(models[[i]]@background@data)
    res[i, 2] <- get_model_reg(models[[i]])
    fcs[i] <- get_model_fc(models[[i]])
    res[i, 4] <- metrics[[1]][i]
    if (metric != "aicc")
      res[i, 5] <- metrics[[2]][i]
  }

  if (metric != "aicc") {
    res[, 6] <- res[, 4] - res[, 5]
  } else {
    res[, 5] <- res[, 4] - min(res[, 4])
  }
  res <- as.data.frame(res)
  colnames(res) <- labels
  res$fc <- fcs
  output <- SDMtune(results = res, models = models)
  return(output)
}
