#' MaxEnt Species With Data
#'
#' Object similar to the MAxEnt SWD format that hosts the species name, the coordinates
#' of the locations and the value of the environmental variables at the location places.
#'
#' @slot species character. Name of the species.
#' @slot coords data.frame. Coordinates of the locations.
#' @slot data data.frame. Value of the environmental variables at location places.
#' and "Test".
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
            cat("Class            :", class(object), "\n")
            cat("Species          :", object@species, "\n")
            cat("Locations        :", nrow(object@data), "\n")
            cat("Variables        :", names(Filter(is.numeric, object@data)), "\n")
            cat("Categoricals     :", names(Filter(is.factor, object@data)))
          })

#' Prepare a SWD data set for MaxEnt models
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function prepares a data frame in the SWD format (sample with data).
#'
#' @param species The name of the species, background in the case of background locations.
#' @param coords The coordinates of the presence or background locations.
#' @param env The environmental variables in a RasterStack format.
#' @param categoricals Vector indicating which of the environmental variable is categorical, default is NULL.
#'
#' @return A SWD object
#'
#' @examples
#' \dontrun{
#' presence_swd <- prepareSWD(species = "Bradipus variegatus", coords = presence, env = env)}
#'
#' @author Sergio Vignali
prepareSWD <- function(species, coords, env, categoricals = NULL) {

  coords <- as.data.frame(coords)
  colnames(coords) <- c("LON", "LAT")

  message(paste0("Extracting environmental condition for ", species, "..."))
  data <- as.data.frame(extract(env, coords))

  is_na <- is.na(rowSums(data))
  if (any(is_na)) {
    # Remove any occurrence point with NA for at least one variable
    data <- data[-which(is_na), ]
    coords <- coords[-which(is_na), ]
    discarded_locations <- length(is_na[is_na == TRUE])
    message(paste("Warning:",
                  discarded_locations,
                  ifelse(discarded_locations == 1, "location is", "locations are"),
                  "NA for some environmental variables,",
                  ifelse(discarded_locations == 1, "it", "they"),
                  "will be discard!")
            )
  }
  # Set categorical variables as factors
  colnames(coords) <- c("X", "Y")
  if (!is.null(categoricals)) {
    for (i in categoricals) {
      data[, i] <- as.factor(data[, i])
    }
}
  swd <- SWD(species = species, coords = coords, data = data)
  return(swd)
}
