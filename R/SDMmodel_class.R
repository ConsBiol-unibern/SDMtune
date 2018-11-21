setClassUnion("model", c("Maxent", "Maxnet", "NN"))
#' SDMmodel
#'
#' This Class represents a MaxEnt model objects and hosts all the information
#' related to the model.
#'
#' @slot presence SWD. The presence locations used to train the model.
#' @slot background SWD. The backgorund locations used to train the model.
#' @slot model Maxent or Maxnet object.
#' @slot html character. The path of the html file, available only after running
#' the function \link{modelReport}.
#'
#' @include SWD_class.R Maxent_class.R Maxnet_class.R NN_class.R
#' @name SDMmodel-class
#' @rdname SDMmodel-class
#' @exportClass SDMmodel
#'
#' @author Sergio Vignali
SDMmodel <- setClass("SDMmodel",
  representation(
    presence = "SWD",
    background = "SWD",
    model = "model",
    html = "character"),
  validity = function(object)	{
    if (ncol(object@presence@data) != ncol(object@background@data))
      return("presence and background have a different number of columns!")

    if (length(setdiff(sort(colnames(object@presence@data)),
                            sort(colnames(object@background@data)))) > 0)
      return("presence and background have different variables!")

    return(TRUE)
 }
)

setMethod("show",
  signature = "SDMmodel",
  definition = function(object) {
    cat("Class                :", class(object), "\n")
    cat("Model                :", class(object@model), "\n")
    cat("Species              :", object@presence@species, "\n")
    cat("Presence data        :", nrow(object@presence@data), "\n")
    cat("Background data      :", nrow(object@background@data), "\n")
    cat("Continuous variables :", names(Filter(is.numeric, object@presence@data)), "\n")
    cat("Categorical variables:", names(Filter(is.factor, object@presence@data)))

    if (!identical(object@html, character(0)))
      browseURL(object@html)
  }
)
#' Plot SDMmodel object
#'
#' Used only for the objects containing NN model. It plots the auc values for
#' all the epoch if monitored during training.
#'
#' @exportMethod plot
#' @importFrom plotly %>% plot_ly add_trace layout
#'
#' @author Sergio Vignali
setMethod("plot",
          signature(x = "SDMmodel", y = "missing"),
          definition = function(x) {

            if (class(x@model) != "NN") {
              stop(paste("No plot method for model of class",
                         class(x@model)))
            }

            if (class(x@model) == "NN" & length(x@model@train_aucs) == 0)
              stop("You must train the model using monitor_auc = TRUE!")

            p <- plot_ly(x = seq(1, x@model@epoch)) %>%
              add_trace(y = x@model@train_aucs, name = "training",
                        mode = "line-basic", type = "scatter") %>%
              plotly::layout(xaxis = list(zeroline = FALSE, title = "epoch"),
                             yaxis = list(title = "auc"),
                             legend = list(orientation = "h"))

            if (!is.null(x@model@val_aucs))
              p <- p %>% add_trace(y = x@model@val_aucs, name = "validation",
                                   mode = "line-basic", type = "scatter")

            return(p)
          })

