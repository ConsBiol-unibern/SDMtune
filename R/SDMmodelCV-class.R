#' SDMmodelCV
#'
#' This Class represents a SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot data \code{\linkS4class{SWD}} object. Full dataset used to make the
#' partitions.
#' @slot folds list with two matrices, the first for the training and the second
#' for the testing dataset. Each column of one matrix represents a fold with
#' \code{TRUE} for the locations included in and \code{FALSE} excluded from the
#' partition.
#'
#' @include SWD-class.R
#' @name SDMmodelCV-class
#' @rdname SDMmodelCV-class
#' @exportClass SDMmodelCV
#'
#' @author Sergio Vignali
SDMmodelCV <- setClass("SDMmodelCV",
                       representation(
                         models = "list",
                         data = "SWD",
                         folds = "list"
                       )
)

setMethod(
  "show",
  signature = "SDMmodelCV",
  definition = function(object) {

    tunable_hypers <- get_tunable_args(object@models[[1]])
    cont_vars <- names(Filter(is.numeric, object@data@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@data@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n")
    cat("Method:", class(object@models[[1]]@model), "\n\n")

    cat("Species:", object@data@species, "\n")
    cat("Replicates:", length(object@models), "\n")
    cat("Presence locations:", nrow(.get_presence(object@data)), "\n")
    cat("Absence locations:", nrow(.get_absence(object@data)), "\n\n")

    cat("Model configurations:\n")
    cat("--------------------\n")

    for (i in 1:length(tunable_hypers)) {
      if (tunable_hypers[i] == "a") {
        next()
      } else {
        h <- slot(object@models[[1]]@model, tunable_hypers[i])
        cat(tunable_hypers[i], ": ", h, "\n", sep = "")
      }
    }

    cat("\nVariables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  }
)
