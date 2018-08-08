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
            cat("An object of class        : ", class(object), "\n")
            cat("Number of iterations      : ", paste(as.character(unique(object@results$it)), collapse = ", "), "\n")
            cat("Number of backgrounds     : ", paste(as.character(unique(object@results$bg)), collapse = ", "), "\n")
            cat("Regularization multipliers: ", paste(as.character(unique(object@results$rm)), collapse = ", "), "\n")
            cat("Feature Classes           : ", paste(unique(object@results$fc), collapse = ", "), "\n")
          })
