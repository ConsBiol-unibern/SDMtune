#' Prepare an SWD object
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function creates a \linkS4class{SWD} object (sample with data).
#'
#' @param species character. The name of the species.
#' @param coords data.frame. The coordinates of the presence or
#' absence/background locations.
#' @param env \code{\link[raster]{stack}} containing the environmental variables
#' used to extract the values at coordinate locations.
#' @param categorical vector indicating which of the environmental variable are
#' categorical, default is \code{NULL}.
#'
#' @return A \linkS4class{SWD} object
#' @export
#' @importFrom stats complete.cases
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' presence
prepareSWD <- function(species, coords, env, categorical = NULL) {

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
  if (!is.null(categorical)) {
    for (i in categorical) {
      data[, i] <- as.factor(data[, i])
    }

  # Reset row names
  rownames(coords) <- NULL
  rownames(data) <- NULL
}
  swd <- SWD(species = species, coords = coords, data = data)
  return(swd)
}
