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
      footer <- c(footer, paste0("bg", ": ", nrow(tuned_args[[i]]@data)))
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

# .get_rank_index <- function(metric, metrics) {
#   if (metric == "aicc") {
#     # The best model is the one with the lowest AICc
#     index <- order(metrics[[1]])
#   } else {
#     # The best model is the one with the highest AUC or TSS
#     diff_metric <- metrics[[1]] - metrics[[2]]
#     # Check if the models are all overfitting the validation dataset
#     if (!any(diff_metric > 0))
#       return(FALSE)
#     # Ordered index of dereasing validation metric
#     o <- order(-metrics[[2]])
#     # Good models are those not overfitting the validation dataset
#     good_models <- o[o %in% which(diff_metric > 0)]
#     # Bad models have diff_metric >= 0
#     bad_models <- o[!o %in% good_models]
#     # Ordered index of decreasomg diff_metric
#     odm <- order(-diff_metric)
#     # Ordered index of bad_models from the one less overfitting
#     bad_models <- odm[odm %in% bad_models]
#     # Combine indexes
#     index <- c(good_models, bad_models)
#   }
#   return(index)
# }

# .get_mutation_options <- function(mother, father, bgs, fcs, regs) {
#   options <- c()
#
#   if (length(regs) >= 1)
#     options <- c(options, "reg")
#
#   if (length(fcs) >= 1)
#     options <- c(options, "fc")
#
#   if (length(bgs) >= 1)
#     options <- c(options, "bg")
#
#   return(options)
# }
#
# .check_hyperparams_validity <- function(bgs, fcs, regs) {
#   l_bgs <- length(bgs) > 1
#   l_fcs <- length(fcs) > 1
#   l_regs <- length(regs) > 1
#
#   if (sum(l_bgs, l_fcs, l_regs) < 2) {
#     stop(paste("You must provide at least two hyperparameters to be tuned!",
#                "Use one of the tune functions to tune only one parameter."))
#   }
# }

.create_optimise_output <- function(models, metric, train_metric, val_metric) {

  labels <- .get_tune_labels(metric)

  res <- matrix(nrow = length(models), ncol = length(labels))
  fcs <- vector("character", length = length(models))

  for (i in 1:length(models)) {
    res[i, 1] <- nrow(models[[i]]@a@data)
    res[i, 2] <- .get_model_reg(models[[i]])
    fcs[i] <- .get_model_fc(models[[i]])
    res[i, 4] <- train_metric[i, 2]
    if (metric != "aicc")
      res[i, 5] <- val_metric[i, 2]
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

.create_chart <- function(folder, script, settings, data, height = 300) {

  dir.create(folder)

  # Copy libraries and style files in lib directory
  dir.create(file.path(folder, "lib"))
  files <- list.files(system.file("lib", package = "SDMtune"),
                      full.names = TRUE)
  file.copy(files, file.path(folder, "lib"))

  # Copy chart template
  file.copy(file.path(system.file("templates", package = "SDMtune"),
                      "chart_template.html"),
            folder)

  # render script
  .render_script(folder, script, settings, data)

  path <- file.path(folder, "chart_template.html")
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(path, height = height)  # Show chart in viewer pane
  } else {
    .start_server(folder, "/chart.html")  # Show chart in browser
  }
  Sys.sleep(.1)
}

#' @importFrom jsonlite toJSON
#' @importFrom whisker whisker.render
.render_script <- function(folder, script, settings, data) {

  template <- paste(readLines(file.path(system.file("scripts",
                                                    package = "SDMtune"),
                                        script),
                              encoding = "UTF-8"),
                    collapse = "\n")

  data <- list(settings = jsonlite::toJSON(settings),
               data = jsonlite::toJSON(data))

  rendered_script <- whisker::whisker.render(template, data = data)
  writeLines(rendered_script, file.path(folder, "lib", "chart_script.js"))
}

#' @importFrom jsonlite write_json
.update_chart <- function(folder, data) {
  jsonlite::write_json(data, file.path(folder, "data.json"))
}

.get_train_args <- function(model) {

  args <- list(p = model@p, a = model@a)

  if (class(model) == "SDMmodelCV") {
    args$replicates <- length(model@models)
    args$folds <- model@folds
    model <- model@models[[1]]@model
  } else {
    args$replicates <- 1
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
#' @param model \link{SDMmodel} or \link{SVDmodelCV} object.
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
