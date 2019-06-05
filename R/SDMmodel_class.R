setClassUnion("model", c("Maxent", "Maxnet"))
#' SDMmodel
#'
#' This Class represents a SDMmodel model object and hosts all the information
#' related to the model.
#'
#' @slot p \link{SWD} object. The presence locations used to train the model.
#' @slot a \link{SWD} object. The absence or background locations used to train
#' the model.
#' @slot model \link{Maxent} or \link{Maxnet} object.
#'
#' @include SWD_class.R Maxent_class.R Maxnet_class.R
#' @exportClass SDMmodel
#'
#' @author Sergio Vignali
SDMmodel <- setClass("SDMmodel",
  representation(
    p = "SWD",
    a = "SWD",
    model = "model"),
  validity = function(object)	{
    if (ncol(object@p@data) != ncol(object@a@data))
      return("Arguments 'p' and 'a' have a different number of columns!")

    if (length(setdiff(sort(colnames(object@p@data)),
                            sort(colnames(object@a@data)))) > 0)
      return("Arguments 'p' and 'a' have different variables!")

    return(TRUE)
 }
)

setMethod(
  "show",
  signature = "SDMmodel",
  definition = function(object) {

    tunable_hypers <- get_tunable_args(object)
    cont_vars <- names(Filter(is.numeric, object@p@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@p@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n")
    cat("Method:", class(object@model), "\n\n")

    cat("Species:", object@p@species, "\n")
    cat("Presence locations:", nrow(object@p@data), "\n\n")

    cat("Model configurations:\n")
    cat("--------------------\n")

    for (i in 1:length(tunable_hypers)) {
      if (tunable_hypers[i] == "a") {
        h <- nrow(object@a@data)
      } else {
        h <- slot(object@model, tunable_hypers[i])
      }
      cat(tunable_hypers[i], ": ", h, "\n", sep = "")
    }

    cat("\nVariables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  }
)
