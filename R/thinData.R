#' Thin Data
#'
#' Remove all but one location per raster cell. The function removes NAs and if
#' more than one location falls within the same raster cell it selects randomly
#' one.
#'
#' @param coords data.frame or matrix with the coordinates, see details.
#' @param x character. Name of the column containing the x coordinates, default
#' is "x".
#' @param y character. Name of the column containing the y coordinates, default
#' is "y".
#' @param env \code{\link[raster]{stack}} containing the environmental
#' variables, or a single \code{\link[raster]{raster}} layer.
#'
#' @details * **coords** and **env** must have the same coordinate reference
#' system.
#' * The \code{coords} argument can contain several columns. This is useful if
#' the user has information related to the coordinates that doesn't want to
#' loose with the thinning procedure. The function expects to have the x
#' coordinates in a column named "x", and the y coordinates in a column named
#' "y". If this is not the case, the name of the columns containing the
#' coordinates can be specified using the arguments \code{x} and \code{y}.
#'
#' @return a matrix or a data frame with the thinned locations.
#' @export
#' @importFrom raster cellFromXY extract
#' @importFrom stats complete.cases
#' @importFrom progress progress_bar
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 9000)
#' nrow(bg_coords)
#'
#' # Thin the locations
#' # There are probably few coordinates that have NAs for some predictors, the
#' # function will remove these coordinates. Note that the finction expects to
#' # the coordinates in two column named "x" and "y"
#' colnames(bg_coords)
#' thinned_bg <- thinData(bg_coords, env = predictors)
#' nrow(thinned_bg)
#'
#' # Here we double the coordinates and run the function again
#' thinned_bg <- thinData(rbind(bg_coords, bg_coords), env = predictors)
#' nrow(thinned_bg)
#'
#' # In case of a dataframe containing more than two columns (e.g. a dataframe
#' # with the coordinates plus an additional column with the age of the species)
#' # and custom column names, use the function in this way
#' age <- sample(c(1, 2), size = nrow(bg_coords), replace = TRUE)
#' data <- cbind(age, bg_coords)
#' colnames(data) <- c("age", "X", "Y")
#' thinned_bg <- thinData(data, env = predictors, x = "X", y = "Y")
#' head(data)
#' }
thinData <- function(coords, env, x = "x", y = "y") {

  # Check if columns with coordinates are present in coords
  if (!x %in% colnames(coords))
    stop("The column '", x, "' is not present, please provide the correct name",
         " for the column containing the x coordinates.")

  if (!y %in% colnames(coords))
    stop("The column '", y, "' is not present, please provide the correct name",
         " for the column containing the y coordinates.")

  # Copy the data
  data <- coords
  coords <- data[, c(x, y)]

  # Remove coords where env are NA
  index <- complete.cases(raster::extract(env, coords))
  coords <- coords[index, ]
  data <- data[index, ]
  # Get the relative cell numbers of the coordinates
  cells <- raster::cellFromXY(env, coords)

  unique_cells <- unique(cells)
  l <- length(unique_cells)

  index <- c()

  pb <- progress::progress_bar$new(
    format = "Thin Data [:bar] :percent in :elapsedfull",
    total = l, clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  for (i in 1:l) {
    selection <- which(cells == unique_cells[i])
    if (length(selection) > 1) {
      index <- c(index, sample(selection, size = 1))
    } else {
      index <- c(index, selection)
    }
    pb$tick(1)
  }
  output <- data[index, ]
  rownames(output) <- NULL

  return(output)
}
