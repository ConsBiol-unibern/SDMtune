#' Prepare a SWD data set for MaxEnt models
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function prepares a data frame in the SWD format (sample with data).
#'
#' @param species character. The name of the species.
#' @param coords data.frame. The coordinates of the presence or background locations.
#' @param env \link{stack} or \link{brick} containing the environmental variables used to train the model.
#' @param categoricals vector indicating which of the environmental variable are categoricals, default is NULL.
#'
#' @return A SWD object
#' @export
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
  data <- as.data.frame(raster::extract(env, coords))

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
