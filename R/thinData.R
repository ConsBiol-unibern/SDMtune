#' Thin Data
#'
#' Remove all but one location per raster cell. The function removes NAs and if
#' more than one location falls within the same raster cell it selects randomly
#' one.
#'
#' @param coords data.frame or matrix with the coordinates, see details.
#' @param x character. Name of the column containing the x coordinates.
#' @param y character. Name of the column containing the y coordinates.
#' @param env \link[terra]{rast} containing the environmental variables.
#' @param verbose logical, if `TRUE` prints an informative message.
#' @param progress logical, if `TRUE` shows a progress bar.
#'
#' @details
#' * **coords** and **env** must have the same coordinate reference
#' system.
#' * The `coords` argument can contain several columns. This is useful if the
#' user has information related to the coordinates that doesn't want to loose
#' with the thinning procedure. The function expects to have the x coordinates
#' in a column named "x", and the y coordinates in a column named "y". If this
#' is not the case, the name of the columns containing the coordinates can be
#' specified using the arguments `x` and `y`.
#'
#' @return a matrix or a data frame with the thinned locations.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{# Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare background locations, by sampling  also on areas with NA values
#' bg_coords <- terra::spatSample(predictors,
#'                                size = 9000,
#'                                method = "random",
#'                                xy = TRUE,
#'                                values = FALSE)
#' nrow(bg_coords)
#'
#' # Thin the locations
#' # The function will remove the coordinates that have NA values for some
#' # predictors. Note that the function expects to have the coordinates in two
#' # columns named "x" and "y"
#'
#' colnames(bg_coords)
#' thinned_bg <- thinData(bg_coords,
#'                        env = predictors)
#' nrow(thinned_bg)
#'
#' # Here we sample only on areas without NA values and then we double the
#' # coordinates
#' bg_coords <- terra::spatSample(predictors,
#'                                size = 9000,
#'                                method = "random",
#'                                na.rm = TRUE,
#'                                xy = TRUE,
#'                                values = FALSE)
#'
#' thinned_bg <- thinData(rbind(bg_coords, bg_coords),
#'                        env = predictors)
#'
#' nrow(thinned_bg)
#'
#' # In case of a dataframe containing more than two columns (e.g. a dataframe
#' # with the coordinates plus an additional column with the age of the species)
#' # and custom column names, use the function in this way
#' age <- sample(c(1, 2),
#'               size = nrow(bg_coords),
#'               replace = TRUE)
#'
#' data <- cbind(age, bg_coords)
#' colnames(data) <- c("age", "X", "Y")
#'
#' thinned_bg <- thinData(data,
#'                        env = predictors,
#'                        x = "X",
#'                        y = "Y")
#' head(data)}
thinData <- function(coords,
                     env,
                     x = "x",
                     y = "y",
                     verbose = TRUE,
                     progress = TRUE) {

  # Check if columns with coordinates are present in coords
  if (!x %in% colnames(coords))
    cli::cli_abort(c(
      "!" = "Column {.val {x}} does not exist",
      "i" = "Please use the name of the column with the {.val x} coordinates."
    ))

  if (!y %in% colnames(coords))
    cli::cli_abort(c(
      "!" = "Column {.val {y}} does not exist",
      "i" = "Please use the name of the column with the {.val y} coordinates."
    ))

  # TODO: Remove with version 2.0.0
  if (inherits(env, "Raster")) {
    .raster_error("rast")
  }

  if (!inherits(env, "SpatRaster"))
    cli::cli_abort(c(
      "!" = "{.var env} must be a {.cls SpatRaster} object",
      "x" = "You have supplied a {.cls {class(env)}} instead."
    ))

  # Copy the data
  data <- coords
  coords <- data[, c(x, y)]

  # Remove coords where env are NA
  idx <- stats::complete.cases(terra::extract(env, coords))
  coords <- coords[idx, ]
  data <- data[idx, ]
  # Get the relative cell numbers of the coordinates
  cells <- terra::cellFromXY(env, coords)

  unique_cells <- unique(cells)
  l <- length(unique_cells)

  index <- c()

  if (progress)
    cli::cli_progress_bar(
      name = "Thin Data",
      type = "iterator",
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | \\
                ETA: {cli::pb_eta} - {cli::pb_elapsed_clock}",
      total = l,
      clear = FALSE
    )

  for (i in 1:l) {
    selection <- which(cells == unique_cells[i])

    if (length(selection) > 1) {
      index <- c(index, sample(selection, size = 1))
    } else {
      index <- c(index, selection)
    }

    if (progress)
      cli::cli_progress_update()
  }

  output <- data[index, ]
  rownames(output) <- NULL

  if (verbose) {
    n_na <- nrow(data) - length(idx)
    n_dups <- nrow(data) - length(index)
    cli::cli_alert_success(
      paste("Removed {.val {cli::no(n_na)}} NA{?s}",
            "and {.val {cli::no(n_dups)}} duplicated location{?s}")
    )
  }

  output
}
