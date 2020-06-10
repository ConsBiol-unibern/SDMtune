#' Maxent
#'
#' This Class represents a MaxEnt model objects and hosts all the information
#' related to the model.
#'
#' @slot results matrix. The result that usually MaxEnt provide as a csv file.
#' @slot reg numeric. The value of the regularization multiplier used to train
#' the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot iter numeric. The number of iterations used to train the model.
#' @slot extra_args character. Extra arguments used to run MaxEnt.
#' @slot lambdas vector. The lambdas parameters of the model.
#' @slot coeff data.frame. The lambda coefficients of the model.
#' @slot formula formula. The formula used to make prediction.
#' @slot lpn numeric. Linear Predictor Normalizer.
#' @slot dn numeric. Density Normalizer.
#' @slot entropy numeric. The entropy value.
#' @slot min_max data.frame. The minimum and maximum values of the continuous
#' variables, used for clamping.
#'
#' @aliases NULL Maxent-class
#' @export
#'
#' @author Sergio Vignali
Maxent <- setClass("Maxent",
                   slots = c(
                     results = "matrix",
                     reg = "numeric",
                     fc = "character",
                     iter = "numeric",
                     extra_args = "character",
                     lambdas = "vector",
                     coeff = "data.frame",
                     formula = "formula",
                     lpn = "numeric",
                     dn = "numeric",
                     entropy = "numeric",
                     min_max = "data.frame")
                   )

setMethod("show",
          signature = "Maxent",
          definition = function(object) {
            cat("Class     :", class(object), "\n")
            cat("Reg       :", object@reg, "\n")
            cat("FCs       :", object@fc, "\n")
            cat("Iterations:", object@iter, "\n")
          })
