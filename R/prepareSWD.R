#' Prepare a SWD data set
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function creates a \link{SWD} object (sample with data).
#'
#' @param species character. The name of the species.
#' @param coords data.frame. The coordinates of the presence or
#' absence/background locations.
#' @param env \link{stack} containing the environmental variables used to
#' extract the values at coordinate locations.
#' @param categoricals vector indicating which of the environmental variable are
#' categoricals, default is NULL.
#'
#' @return A \link{SWD} object
#' @export
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#' swd <- prepareSWD(species = "Vultur gryphus", coords = p, env = env)}
#'
#' @author Sergio Vignali
prepareSWD <- function(species, coords, env, categoricals = NULL) {

  coords <- as.data.frame(coords)

  message("Extracting predictor information for given locations...")
  data <- as.data.frame(raster::extract(env, coords))

  # Remove any occurrence point with NA for at least one variable
  index <- complete.cases(data)
  discarded_locations <- nrow(data) - sum(index)
  if (discarded_locations > 0) {
    data <- data[index, ]
    coords <- coords[index, ]
    message(paste("Warning:", discarded_locations,
                  ifelse(discarded_locations == 1, "location is",
                         "locations are"),
                  "NA for some environmental variables,",
                  ifelse(discarded_locations == 1, "it", "they"),
                  "will be discard!"))
  }

  colnames(coords) <- c("X", "Y")
  # Set categorical variables as factors
  if (!is.null(categoricals)) {
    for (i in categoricals) {
      data[, i] <- as.factor(data[, i])
    }

  # Reset row names
  rownames(coords) <- NULL
  rownames(data) <- NULL
}
  swd <- SWD(species = species, coords = coords, data = data)
  return(swd)
}
