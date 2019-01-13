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
               "#Bg:", nrow(model@background@data)))
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

.get_tune_labels <- function(metric) {
  if (metric == "auc") {
    labels <- c("train_AUC", "test_AUC", "diff_AUC")
  } else if (metric == "tss") {
    labels <- c("train_TSS", "test_TSS", "diff_TSS")
  } else {
    labels <- c("AICc", "delta_AICc")
  }
  labels <- c("bg", "reg", "fc", labels)

  return(labels)
}

.get_total_models <- function(pop, gen, remaining) {
  tot <- pop + (gen * remaining)

  return(tot)
}

.get_rank_index <- function(metric, metrics) {
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

.create_optimise_output <- function(models, metric, metrics) {
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
    res[i, 2] <- .get_model_reg(models[[i]])
    fcs[i] <- .get_model_fc(models[[i]])
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

.start_server <- function(folder, name) {
  port <- tools::startDynamicHelp(NA)
  url <- paste0("http://127.0.0.1:", port, "/session/", basename(folder), name)
  utils::browseURL(url)
}

.create_chart <- function(template, context, height = 300) {
  # Create and render template for chart
  folder <- tempfile("sdmsel")
  dir.create(folder)
  .render_chart(folder, template, context)

  path <- file.path(folder, "chart.html")
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(path, height = height)  # Show chart in viewer pane
  } else {
    .start_server(folder, "/chart.html")  # Show chart in browser
  }

  return(folder)
}

#' @importFrom whisker whisker.render
.render_chart <- function(folder, template, context) {

  template <- get(template, envir = .sdmsel)
  style <- get("optimiseCss", envir = .sdmsel)
  jQuery <- get("jQuery", envir = .sdmsel)
  chartJs <- get("chartJs", envir = .sdmsel)

  context <- c(context, list(style = style, jQuery = jQuery, chartJs = chartJs))

  html <- whisker::whisker.render(template, data = context)
  writeLines(html, file.path(folder, "chart.html"))
  Sys.sleep(0.2)
}

#' @importFrom jsonlite write_json
.update_chart <- function(folder, data) {
  jsonlite::write_json(data, file.path(folder, "data.json"), auto_unbox = TRUE)
}
