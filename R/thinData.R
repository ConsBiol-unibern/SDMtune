#' Thin Data
#'
#' Remove all but one location per raster cell. The function selects randomly one
#' location if more than one fall within the same raster cell.
#'
#' @param coords data.frame or matrix with the coordinates.
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' or a single raster layer.
#'
#' @details **coords** and **env** must have the same coordinate reference system.
#'
#' @return a data frame with the thinned locations.
#' @export
#' @importFrom raster cellFromXY
#'
#' @examples \dontrun{
#' thinData(my_coords, my_env)}
#'
#' @author Sergio Vignali
thinData <- function(coords, env) {
  cells <- unique(raster::cellFromXY(env, coords))
  output <- matrix(nrow = length(cells), ncol = 2)
  for (i in 1:length(cells)) {
    if (length(which(cells == cells[i])) > 1) {
      index <- sample(nrow(coords[cells == cells[i], ]), 1)
      output[i, 1] <- coords[cells == cells[i], ][index, 1]
      output[i, 2] <- coords[cells == cells[i], ][index, 2]
    } else {
      output[i, 1] <- coords[i, 1]
      output[i, 2] <- coords[i, 2]
    }
  }
  output <- as.data.frame(output)
  colnames(output) <- colnames(coords)

  return(output)
}
