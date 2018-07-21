#' Maxent
#'
#' This Class represents a MaxEnt model objects and hosts all the information related to the model.
#'
#' @slot presence SWD. The presence locations used to train the model.
#' @slot background SWD. The backgorund locations used to train the model.
#' @slot test SWD. The test locations used to validate the model.
#' @slot results matrix. The result that usually MaxEnt provide as a csv file.
#' @slot rm numeric. The value of the regularization multiplier used to train the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot iterations numeric. The number of iterations used to train the model.
#' @slot type character. The output format of the model.
#' @slot lambdas vector. The lambdas parameters of the model.
#' @slot coeff data.frame. The lambda coefficients of the model.
#' @slot formula formula. The formula used to make prediction.
#' @slot lpn numeric. Linear Predictor Normalizer.
#' @slot dn numeric. Density Normalizer.
#' @slot entropy numeric. The entropy value.
#' @slot min_max data.frame. The minimum and maximum values of the continuous variables, used for clamping.
#' @slot folder character. The path for the folder where are saved all the files produced by MaxEnt,
#' available if the "folder" parameter is provided to the runMaxent function.
#'
#' @include SWD_class.R
#' @name Maxent-class
#' @rdname Maxent-class
#' @exportClass Maxent
#'
#' @author Sergio Vignali
Maxent <- setClass("Maxent",
                   slots = c(
                     presence = "SWD",
                     background = "SWD",
                     test = "SWD",
                     results = "matrix",
                     rm = "numeric",
                     fc = "character",
                     iterations = "numeric",
                     type = "character",
                     lambdas = "vector",
                     coeff = "data.frame",
                     formula = "formula",
                     lpn = "numeric",
                     dn = "numeric",
                     entropy = "numeric",
                     min_max = "data.frame",
                     folder = "character")
)

setMethod("show",
          signature = "Maxent",
          definition = function(object) {
            cat("Class               :", class(object), "\n")
            cat("Species             :", object@presence@species, "\n")
            cat("RM                  :", object@rm, "\n")
            cat("FCs                 :", object@fc, "\n")
            cat("Iterations          :", object@iterations, "\n")
            cat('Output type         :', object@type, '\n')
            cat("Presence data       :", nrow(object@presence@data), "\n")
            cat("Background data     :", nrow(object@background@data), "\n")
            cat("Test data           :", nrow(object@test@data), "\n")
            cat("Continuous variables:", names(Filter(is.numeric, object@presence@data)), "\n")
            cat("Categoricals        :", names(Filter(is.factor, object@presence@data)))

            html <- paste0(object@folder, "/species.html")

            if (file.exists(html)) browseURL(html)
          })
