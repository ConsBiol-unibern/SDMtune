#' Sample With Data
#'
#' Object similar to the MaxEnt SWD format that hosts the species name, the
#' coordinates of the locations and the value of the environmental variables at
#' the location places.
#'
#' @slot species character. Name of the species.
#' @slot coords data.frame. Coordinates of the locations.
#' @slot data data.frame. Value of the environmental variables at location
#' sites.
#' @slot pa numeric. Vector with \code{1} for presence and \code{0} for
#' absence/background locations.
#'
#' @details The object can contains presence/absence, presence/background,
#' presence only or absence/background only data. Use the
#' \code{\link{prepareSWD}} function to create the object.
#'
#' @export
#'
#' @author Sergio Vignali
SWD <- setClass("SWD",
                representation(
                  species = "character",
                  coords = "data.frame",
                  data = "data.frame",
                  pa = "numeric"),
                prototype(
                  species = NA_character_,
                  coords = data.frame(),
                  data = data.frame(),
                  pa = numeric()
                ),
                validity = function(object)	{
                  if (nrow(object@coords) != nrow(object@data))
                    return("coords and data have a different number of rows!")
                  # TODO remove if statement
                  if (1 %in% object@pa | 0 %in% object@pa) {
                    if (nrow(object@coords) != length(object@pa))
                      return("coords and pa have different length!")
                  }
                  return(TRUE)
                }
)

setMethod(
  "show",
  signature = "SWD",
  definition = function(object) {
    # TODO Remove this check in a feature release
    if (!.hasSlot(object, "pa"))
      stop("\nThis object was created using SDMtune v <= 0.1.1 and is now ",
          "deprecated.\nCheck the article \"Deprecated objects\" in the ",
          "package website to see how to convert this object into the new ",
          "format.", call. = FALSE)

    cont_vars <- names(Filter(is.numeric, object@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n\n")

    cat("Species:", object@species, "\n")
    cat("Presence locations:",  nrow(object@data[object@pa == 1, ]), "\n")
    cat("Absence locations:",  nrow(object@data[object@pa == 0, ]), "\n\n")

    cat("Variables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars , "\n")
  })
