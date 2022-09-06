#' Model Report
#'
#' Make a report that shows the main results.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param folder character. The name of the folder in which to save the output.
#' The folder is created in the working directory.
#' @param test \linkS4class{SWD} object with the test locations, default is
#' `NULL`.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic", default is
#' \code{NULL}.
#' @param response_curves logical, if `TRUE` it plots the response curves in the
#' html output, default is `FALSE`.
#' @param only_presence logical, if `TRUE` it uses only the range of the
#' presence location for the marginal response, default is `FALSE`.
#' @param jk logical, if `TRUE` it runs the jackknife test, default is `FALSE`.
#' @param env \link[raster]{stack}. If provided it computes and adds a
#' prediction map to the output, default is `NULL`.
#' @param clamp logical for clumping during prediction, used for response curves
#' and for the prediction map, default is `TRUE`.
#' @param permut integer. Number of permutations, default is 10.
#' @param factors list with levels for factor variables,
#' see \link[raster]{predict}
#'
#' @details The function produces a report similar to the one created by MaxEnt
#' software.
#'
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # If you run the following examples with the function example(), you may want
#' # to set the argument ask like following: example("modelReport", ask = FALSE)
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "lq")
#'
#' # Create the report
#' \dontrun{
#' modelReport(model, type = "cloglog", folder = "my_folder", test = test,
#'             response_curves = TRUE, only_presence = TRUE, jk = TRUE,
#'             env = predictors, permut = 2)
#' }
#' }
modelReport <- function(model, folder, test = NULL, type = NULL,
                        response_curves = FALSE, only_presence = FALSE,
                        jk = FALSE, env = NULL, clamp = TRUE, permut = 10,
                        factors = NULL) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg kableExtra} to use this function",
      call = NULL
    )
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg htmltools} to use this function",
      call = NULL
    )
  }

  if (file.exists(file.path(getwd(), folder))) {
    msg <- message(cli::col_red(cli::symbol$fancy_question_mark),
                   " The folder '", folder,
                   "' already exists, do you want to overwrite it?")
    continue <- utils::menu(choices = c("Yes", "No"), title = msg)
  } else {
    continue <- 1
  }
  if (continue == 1) {
    template <- system.file("templates", "modelReport.Rmd", package = "SDMtune")

    folder <- file.path(getwd(), folder)
    dir.create(file.path(folder, "plots"), recursive = TRUE,
               showWarnings = FALSE)
    species <- gsub(" ", "_", tolower(model@data@species))
    title <- paste(class(model@model), "model for", model@data@species)
    args <- c(paste0("--metadata=title:\"", title, "\""))
    output_file <- paste0(species, ".html")

    cli::cli_text(
      "\f",
      cli::rule(
        left = "Model Report - method: {class(model@model)}",
        right = cli::style_italic(model@data@species),
        line_col = "#4bc0c0",
        col = "#f58410",
        width = 80
      )
    )

    rmarkdown::render(template,
                      output_file = output_file,
                      output_dir = folder,
                      params = list(model = model, type = type, test = test,
                                    folder = folder, env = env, jk = jk,
                                    response_curves = response_curves,
                                    only_presence = only_presence,
                                    clamp = clamp, permut = permut,
                                    factors = factors),
                      output_options = list(pandoc_args = args),
                      quiet = TRUE
                      )
    utils::browseURL(file.path(folder, output_file))
  }
}

.save_report_files <- function(params) {
  cli::cli_progress_step("Save files")
  saveRDS(params$model, file = file.path(params$folder, "model.Rds"))
  swd2csv(params$model@data, file.path(params$folder, "train.csv"))
  if (!is.null(params$test))
    swd2csv(params$test, file.path(params$folder, "test.csv"))
}

.plot_report_roc <- function(params, plot_folder) {
  cli::cli_progress_step("Plot ROC curve")
  plot <- plotROC(params$model, test = params$test)
  suppressMessages(
    ggplot2::ggsave(filename = "ROC_curve.png",
                    plot = plot,
                    device = "png",
                    path = plot_folder)
  )
  path <- file.path(plot_folder, "ROC_curve.png")
  element <- paste0("<a href=\"",
                    path, "\"><img src=\"",
                    path, "\" style=\"width: 70%; display: block; margin-left: auto; margin-right: auto;\"></a>")
  htmltools::HTML(element)
}

.compute_report_thresholds <- function(params) {
  cli::cli_progress_step("Compute thresholds")
  knitr::kable(thresholds(params$model, type = params$type, test = params$test),
               digits = 20) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}

.plot_report_response_curves <- function(params, type, plot_folder) {
  cli::cli_progress_step("Plot {type} response curves")
  vars <- sort(colnames(params$model@data@data))
  elements <- c()

  for (var in vars) {
    plot <- plotResponse(params$model, var,
                         type = params$type,
                         only_presence = params$only_presence,
                         marginal = ifelse(type == "marginal", TRUE, FALSE),
                         rug = TRUE,
                         color = "#4bc0c0")
    fname <- paste0(var, "_", type, ".png")
    suppressMessages(ggplot2::ggsave(filename = fname,
                                     plot = plot,
                                     device = "png",
                                     path = plot_folder))
    path <- file.path(plot_folder, fname)
    element <- paste0("<a href=\"",
                      path, "\"><figure><img src=\"",
                      path,
                      "\" title=\"",
                      var,
                      "\"><figcaption>",
                      var,
                      "</figcaption></figure></a>")
    elements <- c(elements, element)
  }

  elements <- paste(elements, collapse = "")

  return(htmltools::HTML(elements))
}

.make_report_prediction <- function(params, plot_folder) {
  cli::cli_progress_step("Predict distribution map")
  pred <- predict(params$model,
                  data = params$env,
                  type = params$type,
                  filename = file.path(params$folder, "map"),
                  overwrite = TRUE,
                  clamp = params$clamp,
                  factors = params$factors)
  plot <- plotPred(pred,
                   lt = params$type,
                   hr = TRUE,
                   colorramp = c("#2c7bb6", "#abd9e9",
                                 "#ffffbf", "#fdae61", "#d7191c"))
  suppressMessages(ggplot2::ggsave(filename = "map.png",
                                   plot = plot,
                                   device = "png",
                                   path = plot_folder))
  path <- file.path(plot_folder, "map.png")
  element <- paste0("<a href=\"",
                    path,
                    "\"><img src=\"",
                    path,
                    "\" style=\"width: 70%; display: block; margin-left: auto; margin-right: auto;\"></a>")

  return(htmltools::HTML(element))
}

.compute_report_variable_importance <- function(params) {
  cli::cli_progress_step("Compute variable importance")
  knitr::kable(suppressMessages(varImp(params$model, params$permut))) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    )
}

.compute_report_jk <- function(params, test, plot_folder) {
  cli::cli_progress_step("Run Jackknife test")
  jk <- suppressMessages(doJk(params$model, metric = "auc", test = params$test))
  plot <- plotJk(jk, type = "train", ref = auc(params$model))
  suppressMessages(ggplot2::ggsave(filename = "train_jk.png",
                                   plot = plot,
                                   device = "png",
                                   path = plot_folder))

  if (!is.null(test)) {
    plot <- plotJk(jk, type = "test", ref = auc(params$model, test))
    suppressMessages(ggplot2::ggsave(filename = "test_jk.png",
                                     plot = plot,
                                     device = "png",
                                     path = plot_folder))
    path1 <- file.path(plot_folder, "train_jk.png")
    path2 <- file.path(plot_folder, "test_jk.png")
    element <- paste0("<a href=\"",
                      path1,
                      "\"><img src=\"",
                      path1,
                      "\" width=50%></a><a href=\"",
                      path2,
                      "\"><img src=\"",
                      path2,
                      "\" width=50%></a>")
  } else {
    element <- paste0("<a href=\"",
                      path1,
                      "\"><img src=\"",
                      path1,
                      "\" style=\"width: 70%; display: block; margin-left: auto; margin-right: auto;\"></a>")
  }

  return(htmltools::HTML(element))
}

.write_report_model_settings <- function(params) {
  cli::cli_progress_step("Write model settings")

  # Train dataset
  text <- paste(
    "* Model type:", class(params$model@model), "\n",
    "* Train locations:", nrow(params$model@data@data), "\n",
    "    * presence:", sum(params$model@data@pa == 1), "\n",
    "    * absence/background:", sum(params$model@data@pa == 0), "\n"
  )

  # Test dataset
  if (!is.null(params$test)) {
    text <- paste(
      text,
      "* Test locations:", nrow(params$test@data), "\n",
      "    * presence:", sum(params$test@pa == 1), "\n",
      "    * absence/background:", sum(params$test@pa == 0), "\n"
    )
  }

  # Variables
  text <- paste(
    text,
    "* Continuous variables:",
    paste(names(Filter(is.numeric, params$model@data@data)), collapse = ", "),
    "\n",
    "* Categorical variables:",
    paste(names(Filter(is.factor, params$model@data@data)), collape = ", "),
    "\n"
  )

  if (inherits(params$model@model, c("Maxent", "Maxnet"))) {
    # Maxent and Maxnet
    text <- paste(
      text,
      "* Output type:", params$type, "\n",
      "* Feature Class combination:", params$model@model@fc, "\n",
      "* Regularization multiplier:", params$model@model@reg, "\n",
      "* Do clamping:", params$clamp, "\n" #TODO: check if predictions
    )

    if (inherits(params$model@model, "Maxent"))
      # Only Maxent
      text <- paste(
        text,
        "* Extra arguments:",
        paste(params$model@model@extra_args, collapse = ", "),
        "\n"
      )
  } else if (inherits(params$model@model, "ANN")) {
    #  ANN
    text <- paste(
      text,
      "* Size:", params$model@model@size, "\n",
      "* Decay:", params$model@model@decay, "\n",
      "* Rang:", params$model@model@rang, "\n",
      "* Maxit:", params$model@model@maxit, "\n"
    )
  } else if (inherits(params$model@model, "BRT")) {
    # BRT
    text <- paste(
      text,
      "* Distribution:", params$model@model@distribution, "\n",
      "* Number of trees:", params$model@model@n.trees, "\n",
      "* Interaction depth:", params$model@model@interaction.depth, "\n",
      "* Shrinkage:", params$model@model@shrinkage, "\n",
      "* Bag fraction:", params$model@model@bag.fraction, "\n"
    )
  } else {
    # RF
    text <- paste(
      text,
      "* Mtry:", params$model@model@mtry, "\n",
      "* Number of trees:", params$model@model@ntree, "\n",
      "* Node size:", params$model@model@nodesize, "\n"
    )
  }

  return(cat(text))
}
