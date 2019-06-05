#' SDMtune class
#'
#' Class used to save the results of one of the following functions:
#' \link{gridSearch}, \link{randomSearch} or \link{optimizeModel}.
#'
#' @slot results data.frame. Results with the evaluation of the models.
#' @slot models list. List of \link{Maxent} objects
#'
#' @export
#'
#' @author Sergio Vignali
SDMtune <- setClass("SDMtune",
                    slots = c(results = "data.frame",
                              models = "list")
                    )

setMethod("show",
  signature = "SDMtune",
  definition = function(object) {

    tunable_hypers <- get_tunable_args(object@models[[1]])

    cat("Object of class: ", class(object), "\n\n")

    cat("Model configurations:\n")
    cat("--------------------\n")

    for (i in 1:length(tunable_hypers)) {
      h <- paste(sort(unique(object@results[, tunable_hypers[i]])),
                 collapse = ", ")
      cat(tunable_hypers[i], ": ", h, "\n", sep = "")
    }
  }
)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot SDMtune object
#'
#' Plot an \link{SDMtune} object. Use the interactive argument to create an
#' interactive chart.
#'
#' @param x \link{SDMtune} object.
#' @param interactive logical, if TRUE plot an interactive chart.
#'
#' @rdname plot-methods
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot aes_string geom_point labs scale_color_manual
#' theme_minimal theme element_text geom_line
#' @exportMethod plot
#'
#' @examples
#' \dontrun{plot(my_SDMtune_object)}
#'
#' @author Sergio Vignali
setMethod("plot",
  signature(x = "SDMtune", y = "missing"),
  definition = function(x, interactive = FALSE) {
    res <- x@results
    models <- x@models

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
    tunable_hypers <- get_tunable_args(models[[1]])
    hyper_cols <- length(tunable_hypers)
    tuned_hypers <- rapply(res[, tunable_hypers], function(x) length(unique(x)))
    #Show line only one one hyper has be tuned
    show_line <- ifelse(sum(tuned_hypers > 1) == 1, TRUE, FALSE)

    x_label <- "model"
    x <- 1:n
    min <- min(x)
    max <- max(x)

    if (interactive) {
      settings <- list(metric = metric,
                       title = "",
                       x_label = x_label,
                       min = min,
                       max = max,
                       labels = x,
                       show_line = show_line,
                       update = FALSE)

      cols <- get_tunable_args(models[[1]])
      grid_footer <- apply(res[, cols], 1,
                           function(x) paste0(names(x), ": ", x,
                                              collapse = "\n"))
      train_metric <- data.frame(x = x, y = res[, hyper_cols + 1])
      if (metric != "AICc") {
        val_metric <- data.frame(x = x, y = res[, hyper_cols + 2])
      } else {
        val_metric <- data.frame(x = NA_real_, y = NA_real_)
      }

      chart_data <- list(train = train_metric, val = val_metric,
                         gridFooter = grid_footer)

      folder <- tempfile("SDMtune")

      .create_chart(folder = folder, script = "gridSearch.js",
                    settings = settings, data = chart_data)
      return(invisible(folder))
    } else {
      if (metric != "AICc") {
        data <- data.frame(x = rep(x, 2),
                           y = c(res[, hyper_cols + 1], res[, hyper_cols + 2]),
                           type = c(rep("Training", n), rep("Validation", n)))
      } else {
        data <- data.frame(x = x,
                           y = res[, hyper_cols + 1],
                           type = rep("Training", n))
      }

      #  Create scatterplot
      p <- ggplot(data, aes_string(x = "x", y = "y", colour = "type",
                                   group = "type")) +
        geom_point() +
        labs(x = x_label, y = metric) +
        scale_color_manual(name = "", values = c("#4bc0c0", "#f58410")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(colour = "#666666", family = "sans-serif"),
              legend.position = "bottom")

      # Add line if is the rusult of a tune function
      if (show_line)
        p <- p + geom_line(linetype = "dashed", size = .3)

      return(p)
    }
  })
