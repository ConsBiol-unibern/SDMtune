#' Thin Data
#'
#' Remove all but one location per raster cell. The function removes NAs and if
#' more than one location falls within the same raster cell it selects randomly
#' one.
#'
#' @param coords data.frame or matrix with the coordinates.
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, or a single raster layer.
#'
#' @details **coords** and **env** must have the same coordinate reference
#' system.
#'
#' @return a data frame with the thinned locations.
#' @export
#' @importFrom raster cellFromXY extract
#' @importFrom stats complete.cases
#'
#' @examples \dontrun{
#' thinData(my_coords, my_env)}
#'
#' @author Sergio Vignali
thinData <- function(coords, env) {

  # Convert coords in matrix
  if (!is.matrix(coords))
    coords <- as.matrix(coords)
  # Get the relative cell numbers of the coordinates
  cells <- raster::cellFromXY(env, coords)
  unique_cells <- unique(cells)
  # Remove cells where coords are NA
  cells <- cells[complete.cases(raster::extract(env, coords))]

  output <- matrix(nrow = length(unique_cells), ncol = 2)
  for (i in 1:length(unique_cells)) {
    if (length(which(cells == unique_cells[i])) > 1) {
      sample <- sample(nrow(coords[cells == unique_cells[i], ]), 1)
      output[i, ] <- unlist(coords[sample, ])
    } else {
      output[i, ] <- unlist(coords[i, ])
    }
  }
  output <- as.data.frame(output)
  colnames(output) <- colnames(coords)

  return(output)
}
