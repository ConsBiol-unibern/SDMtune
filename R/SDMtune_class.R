#' SDMtune class
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
    cat("---------------------\n")

    for (i in 1:length(tunable_hypers)) {
      h <- paste(sort(unique(object@results[, tunable_hypers[i]])),
                 collapse = ", ")
      cat(tunable_hypers[i], ": ", h, "\n", sep = "")
    }
  }
)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot
#'
#' Plot the result of a tune function or of the optimiseModel function, use the
#' interactive argument to create an interactive chart.
#'
#' @param x \link{SDMtune} object.
#' @param interactive logical, if TRUE plot an iteractive chart.
#'
#' @export
#' @docType methods
#' @rdname plot-methods
#' @importFrom ggplot2 ggplot aes_string geom_point labs scale_color_manual
#' theme_minimal theme element_text geom_line
#'
#' @examples
#' \dontrun{plot(my_tune_object)}
#'
#' @author Sergio Vignali
setMethod("plot",
  signature(x = "SDMtune", y = "missing"),
  definition = function(x, interactive = FALSE) {
    res <- x@results
    models <- x@models

    n <- nrow(res)

    # Get metric
    if (colnames(res)[4] == "train_AUC") {
      metric <- "AUC"
    } else if (colnames(res)[4] == "train_TSS") {
      metric <- "TSS"
    } else {
      metric <- "AICc"
    }

    show_line = TRUE

    if (length(unique(res$fc)) == 1 & length(unique(res$reg)) == 1
               & length(unique(res$bg)) != 1) {
      #  Result of tuneBg function
      title <- "Tune Backgrounds"
      x_label <- "backgrounds"
      x <- res[, 1]
      min <- min(x)
      max <- max(x)
    } else if (length(unique(res$fc)) != 1 & length(unique(res$reg)) == 1
               & length(unique(res$a)) == 1) {
      #  Result of tuneFC function
      title <- "Tune Feature Combinations"
      x_label <- "feature combination"
      x <- res[, 3]
      min <- 0
      max <- 1
    } else if (length(unique(res$fc)) == 1 & length(unique(res$reg)) != 1
               & length(unique(res$bg)) == 1) {
      #  Result of tuneReg function
      title <- "Tune Regularization"
      x_label <- "regularization multiplier"
      x <- res[, 2]
      min <- min(x)
      max <- max(x)
    } else {
      #  Result of modelOptimise function
      title <- "Model Optimization"
      x_label <- "model"
      x <- 1:n
      min <- min(x)
      max <- max(x)
      show_line = FALSE
    }

    if (interactive) {
      settings <- list(metric = metric,
                       title = title,
                       x_label = x_label,
                       min = min,
                       max = max,
                       labels = x,
                       show_line = show_line,
                       update = FALSE)

      line_footer <- sapply(models, function(x) .get_footer(x))
      train_metric <- data.frame(x = x, y = res[, 4])
      if (metric != "AICc") {
        val_metric <- data.frame(x = x, y = res[, 5])
      } else {
        val_metric <- data.frame(x = NA_real_, y = NA_real_)
      }

      chart_data <- list(train = train_metric, val = val_metric,
                         lineFooter = line_footer)

      folder <- tempfile("SDMtune")

      .create_chart(folder = folder, script = "tuneModel.js",
                    settings = settings, data = chart_data)
    } else {
      if (metric != "AICc") {
        data <- data.frame(x = rep(x, 2), y = c(res[, 4], res[, 5]),
                           type = c(rep("Training", n), rep("Validation", n)))
      } else {
        data <- data.frame(x = x, y = res[, 4], type = rep("Training", n))
      }

      #  Create scatterplot
      p <- ggplot(data, aes_string(x = "x", y = "y", colour = "type",
                                   group = "type")) +
        geom_point() +
        labs(title = title, x = x_label, y = metric) +
        scale_color_manual(name = "", values = c("#4bc0c0", "#f58410")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(colour = "#666666", family = "sans-serif"),
              legend.position = "bottom")

      # Add line if is the rusult of a tune function
      if (x_label != "model")
        p <- p + geom_line(linetype = "dashed", size = .3)

      return(p)
    }
  })
