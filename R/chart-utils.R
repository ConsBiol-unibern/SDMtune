.create_chart <- function(folder, script, settings, data) {

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
}

#' @importFrom rstudioapi viewer
.show_chart <- function(folder, height = 300) {

  # Show chart if not called from testthat
  if (!Sys.getenv("TESTTHAT") == "true") {
    viewer <- getOption("viewer")
    if (is.null(viewer)) {
      .start_server(folder)
    } else {
      path <- file.path(folder, "chart_template.html")
      rstudioapi::viewer(path, height = height)
    }
    Sys.sleep(.1)
  }
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
.update_data <- function(folder, data) {
  jsonlite::write_json(data, file.path(folder, "data.json"))
  Sys.sleep(.1)
}

#' @importFrom tools startDynamicHelp
#' @importFrom utils browseURL
.start_server <- function(folder) {

  port <- suppressMessages(tools::startDynamicHelp(start = NA))

  if (port != 0) {
    url <- paste0("http://127.0.0.1:", port, "/session/", basename(folder),
                  "/chart_template.html")
    utils::browseURL(url)
  }
  return(invisible(url))
}

.create_plot <- function(x, title, interactive, folder = NULL) {
  res <- x@results
  n <- nrow(res)

  # Get metric
  if (grepl("AUC", paste(colnames(res), collapse = ""))) {
    metric <- "AUC"
  } else if (grepl("TSS", paste(colnames(res), collapse = ""))) {
    metric <- "TSS"
  } else {
    metric <- "AICc"
  }

  # Check how many hypers have be tuned
  tunable_hypers <- get_tunable_args(x@models[[1]])
  hyper_cols <- length(tunable_hypers)
  tuned_hypers <- rapply(res[, tunable_hypers], function(x) length(unique(x)))
  #Show line if only one hyper has be tuned
  show_line <- ifelse(sum(tuned_hypers > 1) == 1, TRUE, FALSE)

  x_labs <- 1:n

  if (interactive) {
    settings <- list(metric = metric,
                     title = title,
                     x_label = "model",
                     min = min(x_labs),
                     max = max(x_labs),
                     labels = x_labs,
                     show_line = show_line,
                     update = FALSE)

    grid_footer <- apply(res[, tunable_hypers], 1,
                         function(x) paste0(names(x), ": ", x,
                                            collapse = "\n"))
    train_metric <- data.frame(x = x_labs, y = res[, hyper_cols + 1])
    if (metric != "AICc") {
      val_metric <- data.frame(x = x_labs, y = res[, hyper_cols + 2])
    } else {
      val_metric <- data.frame(x = NA_real_, y = NA_real_)
    }

    chart_data <- list(train = train_metric, val = val_metric,
                       gridFooter = grid_footer)

    .create_chart(folder = folder, script = "gridSearch.js",
                  settings = settings, data = chart_data)
  } else {
    if (metric != "AICc") {
      data <- data.frame(x = rep(x_labs, 2),
                         y = c(res[, hyper_cols + 1], res[, hyper_cols + 2]),
                         type = c(rep("Training", n), rep("Validation", n)),
                         stringsAsFactors = FALSE)
    } else {
      data <- data.frame(x = x_labs,
                         y = res[, hyper_cols + 1],
                         type = rep("Training", n),
                         stringsAsFactors = FALSE)
    }

    #  Create scatterplot
    p <- ggplot(data, aes_string(x = "x", y = "y", colour = "type",
                                 group = "type")) +
      geom_point() +
      labs(x = "model", y = metric, title = title) +
      scale_color_manual(name = "", values = c("#4bc0c0", "#f58410")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(colour = "#666666"),
            legend.position = "bottom")

    # Add line if is the rusult of a tune function
    if (show_line)
      p <- p + geom_line(linetype = "dashed", size = .3)

    return(p)
  }
}
