#' Thin Data
#'
#' Remove all but one location per raster cell. The function removes NAs and if
#' more than one location falls within the same raster cell it selects randomly
#' one.
#'
#' @param coords data.frame or matrix with the coordinates.
#' @param env \code{\link[raster]{stack}} containing the environmental
#' variables, or a single \code{\link[raster]{raster}} layer.
#'
#' @details **coords** and **env** must have the same coordinate reference
#' system.
#'
#' @return a data frame with the thinned locations.
#' @export
#' @importFrom raster cellFromXY extract
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
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 9000)
#'
#' nrow(bg_coords)
#'
#' # Thin the locations
#' thinned_bg <- thinData(bg_coords, env = predictors)
#' nrow(thinned_bg)
thinData <- function(coords, env) {

  # Convert coords in matrix
  if (!is.matrix(coords))
    coords <- as.matrix(coords)
  # Remove coords where env are NA
  coords <- coords[complete.cases(raster::extract(env, coords)), ]
  # Get the relative cell numbers of the coordinates
  cells <- raster::cellFromXY(env, coords)

  unique_cells <- unique(cells)

  output <- matrix(nrow = length(unique_cells), ncol = 2)

  for (i in 1:length(unique_cells)) {
    if (length(which(cells == unique_cells[i])) > 1) {
      index <- sample(nrow(coords[cells == unique_cells[i], ]), 1)
      output[i, ] <- unlist(coords[cells == unique_cells[i], ][index, ])
    } else {
      output[i, ] <- unlist(coords[cells == unique_cells[i], ])
    }
  }
  output <- as.data.frame(output)
  colnames(output) <- colnames(coords)

  return(output)
}
