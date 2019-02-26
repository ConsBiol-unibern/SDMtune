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

.get_sdmtune_colnames <- function(metric) {
  if (metric == "auc") {
    return(c("train_AUC", "test_AUC", "diff_AUC"))
  } else if (metric == "tss") {
    return(c("train_TSS", "test_TSS", "diff_TSS"))
  } else {
    return(c("AICc", "delta_AICc"))
  }
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
        res[i, "a"] <- nrow(models[[1]]@a@data)
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

.create_model_from_settings <- function(model, settings, bg4test, bg_folds) {

  args <- .get_train_args(model)
  args[names(settings)] <- settings
  if (!is.null(bg_folds)) {
    bg <- bg4test
    bg@data <- bg4test@data[bg_folds[1:settings$bg], ]
    bg@coords <- bg4test@coords[bg_folds[1:settings$bg], ]
    row.names(bg@data) <- NULL
    row.names(bg@coords) <- NULL
    args$a <- bg
  } else {
    args$a <- model@a
  }

  random_model <- do.call("train", args)

  return(random_model)
}
