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
                slots = c(
                  species = "character",
                  coords = "data.frame",
                  data = "data.frame")
)

setMethod("show",
          signature = "SWD",
          definition = function(object) {
            cat("Class                :", class(object), "\n")
            cat("Species              :", object@species, "\n")
            cat("Locations            :", nrow(object@data), "\n")
            cat("Continuous variables :", names(Filter(is.numeric, object@data)), "\n")
            cat("Categorical variables:", names(Filter(is.factor, object@data)))
          })
