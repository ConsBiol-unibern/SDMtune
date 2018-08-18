#' Maxent
#'
#' This Class represents a MaxEnt model objects and hosts all the information related to the model.
#'
#' @slot results matrix. The result that usually MaxEnt provide as a csv file.
#' @slot rm numeric. The value of the regularization multiplier used to train the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot iter numeric. The number of iterations used to train the model.
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
#' @name Maxent-class
#' @rdname Maxent-class
#' @exportClass Maxent
#' @importFrom utils browseURL
#'
#' @author Sergio Vignali
Maxent <- setClass("Maxent",
                   slots = c(
                     results = "matrix",
                     rm = "numeric",
                     fc = "character",
                     iter = "numeric",
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
            cat("Class                :", class(object), "\n")
            cat("RM                   :", object@rm, "\n")
            cat("FCs                  :", object@fc, "\n")
            cat("Iterations           :", object@iter, "\n")
            cat('Output type          :', object@type, '\n')

            html <- list.files(path = object@folder, pattern = ".html",
                               full.names = TRUE)

            if (!identical(html, character(0))) browseURL(html)
          })
