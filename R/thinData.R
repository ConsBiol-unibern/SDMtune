#' Thin Data
#'
#' Remove all but one location per raster cell. The function removes NAs and if
#' more than one location falls within the same raster cell it selects randomly
#' one.
#'
#' @param coords data.frame or matrix with the coordinates.
#' @param env \link{stack} containing the environmental variables, or a single
#' \link{raster} layer.
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
