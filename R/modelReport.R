#' Model Report
#'
#' Make a report that shows the main results.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param folder character. The name of the folder in which to save the output.
#' The folder is created in the working directory.
#' @param test \linkS4class{SWD} object with the test locations.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic".
#' @param response_curves logical, if `TRUE` it plots the response curves in the
#' html output.
#' @param only_presence logical, if `TRUE` it uses only the range of the
#' presence location for the marginal response.
#' @param jk logical, if `TRUE` it runs the jackknife test.
#' @param env \link[terra]{rast}. If provided it computes and adds a prediction
#' map to the output.
#' @param clamp logical for clumping during prediction, used for response curves
#' and for the prediction map.
#' @param permut integer. Number of permutations.
#' @param verbose logical, if `TRUE` prints informative messages.
#'
#' @details The function produces a report similar to the one created by MaxEnt
#' software. See \pkg{terra} documentation to see how to pass factors.
#'
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{# If you run the following examples with the function example(),
#' # you may want to set the argument ask like following: example("modelReport",
#' # ask = FALSE)
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          test = 0.2,
#'                          only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = train,
#'                fc = "lq")
#'
#' # Create the report
#' \dontrun{
#' modelReport(model,
#'             type = "cloglog",
#'             folder = "my_folder",
#'             test = test,
#'             response_curves = TRUE,
#'             only_presence = TRUE,
#'             jk = TRUE,
#'             env = predictors,
#'             permut = 2)}}
modelReport <- function(model,
                        folder,
                        test = NULL,
                        type = NULL,
                        response_curves = FALSE,
                        only_presence = FALSE,
                        jk = FALSE,
                        env = NULL,
                        clamp = TRUE,
                        permut = 10,
                        verbose = TRUE) {

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
    msg <- cli::cli_text(cli::col_red(cli::symbol$fancy_question_mark),
                         " The folder {.file {folder}} already exists,",
                         " do you want to overwrite it?")
    continue <- utils::menu(choices = c("Yes", "No"), title = msg)
  } else {
    continue <- 1
  }

  if (continue == 1) {
    template <- system.file("templates", "modelReport.Rmd", package = "SDMtune")

    folder <- file.path(getwd(), folder)
    plot_folder <- file.path(folder, "plots")
    dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)
    species <- gsub(" ", "_", tolower(model@data@species))
    output_file <- paste0(species, ".html")

    if (verbose)
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
                      params = list(model = model,
                                    type = type,
                                    test = test,
                                    folder = folder,
                                    plot_folder = plot_folder,
                                    env = env,
                                    jk = jk,
                                    response_curves = response_curves,
                                    only_presence = only_presence,
                                    clamp = clamp,
                                    permut = permut,
                                    verbose = verbose),
                      quiet = TRUE
                      )

    utils::browseURL(file.path(folder, output_file))
  }
}

.save_report_files <- function(params) {

  if (params$verbose) {
    cli::cli_text("\n")
    cli::cli_progress_step("Save files")
  }

  saveRDS(params$model, file = file.path(params$folder, "model.Rds"))
  swd2csv(params$model@data, file.path(params$folder, "train.csv"))

  if (!is.null(params$test))
    swd2csv(params$test, file.path(params$folder, "test.csv"))
}

.plot_report_roc <- function(params) {

  if (params$verbose)
    cli::cli_progress_step("Plot ROC curve")

  plot <- plotROC(params$model, test = params$test)
  suppressMessages(ggplot2::ggsave(filename = "ROC_curve.png",
                                   plot = plot,
                                   device = "png",
                                   path = params$plot_folder))
  path <- file.path(params$plot_folder, "ROC_curve.png")
  element <- stringr::str_glue("
    <a href='{path}'>
      <img src='{path}' class='fig-centered'>
    </a>
  ")

  htmltools::HTML(element)
}

.compute_report_thresholds <- function(params) {

  if (params$verbose)
    cli::cli_progress_step("Compute thresholds")

  knitr::kable(thresholds(params$model, type = params$type, test = params$test),
               digits = 20) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}

.plot_report_response_curves <- function(params, type) {

  if (params$verbose)
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
                                     path = params$plot_folder))
    path <- file.path(params$plot_folder, fname)
    element <- stringr::str_glue("
      <a href='{path}'>
        <figure title='{var}'>
          <img src='{path}'>
          <figcaption>{var}</figcaption>
        </figure>
      </a>
    ")
    elements <- c(elements, element)
  }

  # Add p element to reset float
  elements <- paste(c(elements, "<p></p>"), collapse = "")

  htmltools::HTML(elements)
}

.make_report_prediction <- function(params) {

  if (params$verbose)
    cli::cli_progress_step("Predict distribution map")

  pred <- predict(params$model,
                  data = params$env,
                  type = params$type,
                  filename = file.path(params$folder, "map.tif"),
                  overwrite = TRUE,
                  clamp = params$clamp)

  plot <- plotPred(pred,
                   lt = params$type,
                   hr = TRUE,
                   colorramp = c("#2c7bb6", "#abd9e9",
                                 "#ffffbf", "#fdae61", "#d7191c"))

  suppressMessages(ggplot2::ggsave(filename = "map.png",
                                   plot = plot,
                                   device = "png",
                                   path = params$plot_folder))

  path <- file.path(params$plot_folder, "map.png")

  element <- stringr::str_glue("
    <a href='{path}'>
      <img src='{path}' class='fig-centered'>
    </a>
  ")

  htmltools::HTML(element)
}

.compute_report_variable_importance <- function(params) {

  if (params$verbose)
    cli::cli_progress_step("Compute variable importance")

  knitr::kable(varImp(params$model, params$permut, progress = FALSE)) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    )
}

.compute_report_jk <- function(params, test) {

  if (params$verbose)
    cli::cli_progress_step("Run Jackknife test")

  jk <- doJk(params$model, metric = "auc", test = params$test, progress = FALSE)
  plot <- plotJk(jk, type = "train", ref = auc(params$model))
  suppressMessages(ggplot2::ggsave(filename = "train_jk.png",
                                   plot = plot,
                                   device = "png",
                                   path = params$plot_folder))

  if (!is.null(test)) {
    plot <- plotJk(jk, type = "test", ref = auc(params$model, test))
    suppressMessages(ggplot2::ggsave(filename = "test_jk.png",
                                     plot = plot,
                                     device = "png",
                                     path = params$plot_folder))

    path1 <- file.path(params$plot_folder, "train_jk.png")
    path2 <- file.path(params$plot_folder, "test_jk.png")

    element <- stringr::str_glue("
      <a href='{path1}'>
        <img src='{path1}' class='two-figs'>
      </a>
      <a href='{path2}'>
        <img src='{path2}' class='two-figs'>
      </a>
    ")
  } else {
    element <- stringr::str_glue("
      <a href='{path1}'>
        <img src='{path1}' class='fig-centered'>
      </a>
    ")
  }

  htmltools::HTML(element)
}

#' Write Settings
#'
#' Write the settings of a model for the report.
#'
#' @param params list. Parameters passed to the template for the report.
#'
#' @details The functions prints the settings which are captured in the report
#' template.
#'
#' @return invisible `NULL`
#'
#' @noRd
#' @author Sergio Vignali
.write_report_model_settings <- function(params) {

  if (params$verbose)
    cli::cli_progress_step("Write model settings")

  # Train dataset
  text <- stringr::str_glue("

    * Model type: {class(params$model@model)}
    * Train locations: {nrow(params$model@data@data)}
        * presence: {sum(params$model@data@pa == 1)}
        * absence/background: {sum(params$model@data@pa == 0)}
  ")

  # Test dataset
  if (!is.null(params$test)) {
    text <- stringr::str_glue("
      {text}
      * Test locations: {nrow(params$test@data)}
          * presence: {sum(params$test@pa == 1)}
          * absence/background: {sum(params$test@pa == 0)}
    ")
  }

  # Variables
  cont_vars <- paste(names(Filter(is.numeric, params$model@data@data)),
                     collapse = ", ")
  cat_vars <- paste(names(Filter(is.factor, params$model@data@data)),
                    collapse = ", ")
  text <- stringr::str_glue("
    {text}
    * Continuous variables: {cont_vars}
    * Categorical variables: {cat_vars}
  ")

  if (inherits(params$model@model, c("Maxent", "Maxnet"))) {
    # Maxent and Maxnet
    text <- stringr::str_glue("
      {text}
      * Output type: {params$type}
      * Feature Class combination: {params$model@model@fc}
      * Regularization multiplier: {params$model@model@reg}
    ")

    if (!is.null(params$env))
      # Show clamping only if predictions are calculated
      text <- stringr::str_glue("
      {text}
      * Do clamping for predictions: {params$clamp}
    ")

    if (inherits(params$model@model, "Maxent")) {
      # Only Maxent
      extra_args <- paste(params$model@model@extra_args, collapse = ", ")
      text <- stringr::str_glue("
        {text},
        * Extra arguments: {extra_args}
      ")
    }
  } else if (inherits(params$model@model, "ANN")) {
    #  ANN
    text <- stringr::str_glue("
      {text}
      * Size: {params$model@model@size}
      * Decay: {params$model@model@decay}
      * Rang: {params$model@model@rang}
      * Maxit: {params$model@model@maxit}
    ")
  } else if (inherits(params$model@model, "BRT")) {
    # BRT
    text <- stringr::str_glue("
      {text},
      * Distribution: {params$model@model@distribution}
      * Number of trees: {params$model@model@n.trees}
      * Interaction depth: {params$model@model@interaction.depth}
      * Shrinkage: {params$model@model@shrinkage}
      * Bag fraction: {params$model@model@bag.fraction}
    ")
  } else {
    # RF
    text <- stringr::str_glue("
      {text},
      * Mtry: {params$model@model@mtry}
      * Number of trees: {params$model@model@ntree}
      * Node size: {params$model@model@nodesize}
    ")
  }

  cat(text)
}
