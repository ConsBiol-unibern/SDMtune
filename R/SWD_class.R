#' Sample With Data
#'
#' Object similar to the MaxEnt SWD format that hosts the species name, the
#' coordinates of the locations and the value of the environmental variables at
#' the location places.
#'
#' @slot species character. Name of the species.
#' @slot coords data.frame. Coordinates of the locations.
#' @slot data data.frame. Value of the environmental variables at location
#' places.
#'
#' @export
#'
#' @author Sergio Vignali
SWD <- setClass("SWD",
                representation(
                  species = "character",
                  coords = "data.frame",
                  data = "data.frame"),
                prototype(
                  species = NA_character_,
                  coords = data.frame(),
                  data = data.frame()
                ),
                validity = function(object)	{
                  if (nrow(object@coords) != nrow(object@data))
                    return("coords and data have a different number of rows!")

                  return(TRUE)
                }
)

setMethod(
  "show",
  signature = "SWD",
  definition = function(object) {

    cont_vars <- names(Filter(is.numeric, object@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n\n")

    cat("Species:", object@species, "\n")
    cat("Locations:", nrow(object@data), "\n\n")

    cat("Variables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  })
