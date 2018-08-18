setOldClass("maxnet")
#' Maxnet
#'
#' This Class represents a Maxnet model objects and hosts all the information related to the model.
#'
#' @slot rm numeric. The value of the regularization multiplier used to train the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot model maxnet. The maxnet model object.
#'
#' @name Maxnet-class
#' @rdname Maxnet-class
#' @exportClass Maxnet
#'
#' @author Sergio Vignali
Maxnet <- setClass("Maxnet",
                   slots = c(
                     rm = "numeric",
                     fc = "character",
                     model = "maxnet")
)

setMethod("show",
          signature = "Maxnet",
          definition = function(object) {
            cat("Class                :", class(object), "\n")
            cat("RM                   :", object@rm, "\n")
            cat("FCs                  :", object@fc, "\n")
          })
