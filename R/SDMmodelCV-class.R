#' SDMmodelCV
#'
#' This Class represents an SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot data \linkS4class{SWD} object. Full dataset used to make the
#' partitions.
#' @slot folds list with two matrices, the first for the training and the second
#' for the testing dataset. Each column of one matrix represents a fold with
#' `TRUE` for the locations included in and `FALSE` excluded from the partition.
#'
#' @rdname SDMmodelCV-class
#' @include SWD-class.R
#' @export
#'
#' @author Sergio Vignali
SDMmodelCV <- setClass("SDMmodelCV",
                       representation(
                         models = "list",
                         data = "SWD",
                         folds = "list"
                       )
)

#' @param object SDMmodelCV object
#' @rdname SDMmodelCV-class
setMethod(
  "show",
  signature = "SDMmodelCV",
  definition = function(object) {

    tunable_hypers <- getTunableArgs(object@models[[1]])
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

    for (i in seq_along(tunable_hypers)) {
      if (tunable_hypers[i] == "a") {
        next
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
