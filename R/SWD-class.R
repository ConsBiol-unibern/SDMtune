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
#' @slot pa numeric. Vector with `1` for presence and `0` for absence/background
#' locations.
#'
#' @details The object can contains presence/absence, presence/background,
#' presence only or absence/background only data. Use the \link{prepareSWD}
#' function to create the object.
#'
#' @rdname SWD-class
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
                  if (nrow(object@coords) != length(object@pa))
                      return("coords and pa have different length!")
                  return(TRUE)
                }
)

#' @param object SWD object
#' @rdname SWD-class
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

    cli::cli_h2("Object of class: {.cls {class(object)}}")

    cli::cli_h3("Info")

    cli::cli_li("{.field Species}: {.emph {object@species}}")
    cli::cli_li("{.field Presence locations}: {.val {sum(object@pa == 1)}}")
    cli::cli_li("{.field Absence locations}: {.val {sum(object@pa == 0)}}")

    cli::cli_h3("Variables")

    cli::cli_li("{.field Continuous}: {.val {cont_vars}}")
    cli::cli_li("{.field Categorical}: {.val {cat_vars}}")
  })
