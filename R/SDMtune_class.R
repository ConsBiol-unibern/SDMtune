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
    cat("An object of class   : ", class(object), "\n")
    cat("Number of backgrounds: ", paste(as.character(unique(object@results$bg)), collapse = ", "), "\n")
    cat("Regularization       : ", paste(as.character(unique(object@results$reg)), collapse = ", "), "\n")
    cat("Feature Classes      : ", paste(unique(object@results$fc), collapse = ", "), "\n")
  }
)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot
#'
#' Plot the result of a tune function or of the optimiseModel function.
#'
#' @param x SDMtune object.
#'
#' @export
#' @docType methods
#' @rdname plot-methods
#'
#' @examples
#' \dontrun{plot(my_tuneBg)}
#'
#' @author Sergio Vignali
setMethod("plot",
  signature(x = "SDMtune", y = "missing"),
  definition = function(x) {
    res <- x@results
    n <- nrow(res)

    # Get metric
    if (colnames(res)[4] == "train_AUC") {
      metric <- "AUC"
    } else if (colnames(res)[4] == "train_TSS") {
      metric <- "TSS"
    } else {
      metric <- "AICc"
    }

    if (length(unique(res$fc)) != 1 & length(unique(res$bg)) != 1) {
      #  Result of modelOptimise function
      title <- "Model Optimization"
      x_label <- "model"
      x <- 1:n
    } else if (length(unique(res$fc)) == 1 & length(unique(res$bg)) != 1) {
      #  Result of tuneBg function
      title <- "Tune Backgrounds"
      x_label <- "backgrounds"
      x <- res[, 1]
    } else if (length(unique(res$fc)) != 1 & length(unique(res$bg)) == 1) {
      #  Result of tuneFC function
      title <- "Tune Feature Combinations"
      x_label <- "feature combination"
      x <- res[, 3]
    } else {
      #  Result of tuneReg function
      title <- "Tune Regularization"
      x_label <- "regularization multiplier"
      x <- res[, 2]
    }

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
      scale_colour_discrete(name = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

    # Add line if is the rusult of a tune function
    if (x_label != "model")
      p <- p + geom_line()

    return(p)
  })
