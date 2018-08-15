#' Maxnet
#'
#' This Class represents a Maxnet model objects and hosts all the information related to the model.
#'
#' @slot presence SWD. The presence locations used to train the model.
#' @slot background SWD. The backgorund locations used to train the model.
#' @slot test SWD. The test locations used to validate the model.
#' @slot rm numeric. The value of the regularization multiplier used to train the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot type character. The output format of the model.
#' @slot lambdas vector. The lambdas parameters of the model.
#' @slot coeff data.frame. The lambda coefficients of the model.
#' @slot formula formula. The formula used to make prediction.
#' @slot entropy numeric. The entropy value.
#' @slot min_max data.frame. The minimum and maximum values of the continuous variables, used for clamping.
#'
#' @include SWD_class.R
#' @name Maxnet-class
#' @rdname Maxnet-class
#' @exportClass Maxnet
#'
#' @author Sergio Vignali
Maxnet <- setClass("Maxnet",
                   slots = c(
                     presence = "SWD",
                     background = "SWD",
                     test = "SWD",
                     rm = "numeric",
                     fc = "character",
                     type = "character",
                     lambdas = "vector",
                     coeff = "data.frame",
                     formula = "formula",
                     entropy = "numeric",
                     min_max = "data.frame")
)

setMethod("show",
          signature = "Maxnet",
          definition = function(object) {
            cat("Class                :", class(object), "\n")
            cat("Species              :", object@presence@species, "\n")
            cat("RM                   :", object@rm, "\n")
            cat("FCs                  :", object@fc, "\n")
            cat('Output type          :', object@type, '\n')
            cat("Presence data        :", nrow(object@presence@data), "\n")
            cat("Background data      :", nrow(object@background@data), "\n")
            cat("Test data            :", nrow(object@test@data), "\n")
            cat("Continuous variables :", names(Filter(is.numeric, object@presence@data)), "\n")
            cat("Categorical variables:", names(Filter(is.factor, object@presence@data)))
          })
