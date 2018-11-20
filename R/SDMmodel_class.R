setClassUnion("model", c("Maxent", "Maxnet", "NN"))
#' SDMmodel
#'
#' This Class represents a MaxEnt model objects and hosts all the information related to the model.
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

setGeneric("plot", function(object, ...)
  standardGeneric("plot")
)


#' @exportMethod plot
#'
#' @author Sergio Vignali
setMethod("plot",
          signature = "SDMmodel",
          definition = function(object) {

            if (class(object@model) != "NN") {
              stop(paste("No plot method for model of class", class(object@model)))
            }

            df <- data.frame(epoch = 0:499, train = object@model@train_aucs,
                             val = object@model@val_aucs)

            plot_ly(df, x = ~epoch) %>% add_trace(y = ~train, name = "train", mode = "line-basic", type = "scatter") %>%
              add_trace(y = ~val, name = "val", mode = "line-basic", type = "scatter") %>%
              layout(xaxis = list(zeroline = FALSE), yaxis = list(title = "auc"))
          })

