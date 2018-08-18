#' Species With Data
#'
#' Object similar to the MAxEnt SWD format that hosts the species name, the coordinates
#' of the locations and the value of the environmental variables at the location places.
#'
#' @slot species character. Name of the species.
#' @slot coords data.frame. Coordinates of the locations.
#' @slot data data.frame. Value of the environmental variables at location places.
#' and "Test".
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

setMethod("show",
          signature = "SWD",
          definition = function(object) {

            cont_vars <- names(Filter(is.numeric, object@data))
            if (identical(cont_vars, character(0)))
              cont_vars <- NA_character_
            cat_vars <- names(Filter(is.factor, object@data))
            if (identical(cat_vars, character(0)))
              cat_vars <- NA_character_

            cat("Class                :", class(object), "\n")
            cat("Species              :", object@species, "\n")
            cat("Locations            :", nrow(object@data), "\n")
            cat("Continuous variables :", cont_vars, "\n")
            cat("Categorical variables:", cat_vars)
          })
