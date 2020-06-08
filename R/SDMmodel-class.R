setClassUnion("model", c("Maxent", "Maxnet", "ANN", "RF", "BRT"))
#' SDMmodel
#'
#' This Class represents an SDMmodel object and hosts all the information
#' related to the model.
#'
#' @slot data \linkS4class{SWD} object. The data used to train the model.
#' @slot model An object of class \linkS4class{ANN}, \linkS4class{BRT},
#' \linkS4class{RF}, \linkS4class{Maxent} or \linkS4class{Maxnet}.
#'
#' @aliases NULL SDMmodel-class
#' @include SWD-class.R Maxent-class.R Maxnet-class.R RF-class.R BRT-class.R
#' ANN-class.R
#' @export
#'
#' @author Sergio Vignali
SDMmodel <- setClass("SDMmodel",
  representation(
    data = "SWD",
    model = "model")
)

setMethod(
  "show",
  signature = "SDMmodel",
  definition = function(object) {

    tunable_hypers <- getTunableArgs(object)
    cont_vars <- names(Filter(is.numeric, object@data@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@data@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n")
    cat("Method:", class(object@model), "\n\n")

    cat("Species:", object@data@species, "\n")
    cat("Presence locations:", nrow(.get_presence(object@data)), "\n")
    cat("Absence locations:", nrow(.get_absence(object@data)), "\n\n")

    cat("Model configurations:\n")
    cat("--------------------\n")

    for (i in seq_along(tunable_hypers)) {
      if (tunable_hypers[i] == "a") {
        next
      } else {
        h <- slot(object@model, tunable_hypers[i])
        cat(tunable_hypers[i], ": ", h, "\n", sep = "")
      }
    }

    cat("\nVariables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  }
)
