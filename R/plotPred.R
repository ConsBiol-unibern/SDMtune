#' Plot Prediction
#'
#' Plot Prediction output.
#'
#' @param map \link[raster]{raster} object with the prediction.
#' @param lt character. Legend title.
#' @param colorramp vector. A custom colour ramp given as a vector of colours
#' (see example), default is `NULL` and uses a blue/red colour ramp.
#' @param hr logical. If `TRUE` produces an output with high resolution.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @seealso \link{plotPA}.
#'
#' @examples
#' \donttest{
#' map <- terra::rast(matrix(runif(400, 0, 1),
#'                           nrow = 20,
#'                           ncol= 20))
#'
#' plotPred(map,
#'          lt = "Habitat suitability \ncloglog")
#'
#' # Custom colors
#' plotPred(map,
#'          lt = "Habitat suitability",
#'          colorramp = c("#2c7bb6", "#ffffbf", "#d7191c"))
#' }
plotPred <- function(map,
                     lt = "",
                     colorramp = NULL,
                     hr = FALSE) {

  if (!requireNamespace("rasterVis", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg rasterVis} to use this function",
      call = NULL
    )
  }

  # TODO: Remove with version 2.0.0
  if (inherits(map, "RasterLayer")) {
    .raster_error("rast")
  }

  if (!inherits(map, "SpatRaster"))
    cli::cli_abort(c(
      "!" = "{.var map} must be an {.cls SpatRaster} object",
      "x" = "You have supplied a {.cls {class(map)}} instead."
    ))

  if (is.null(colorramp))
    colorramp <- c("blue", "cyan", "green", "yellow", "red")

  if (hr) {
    maxpixels <- terra::ncell(map)
  } else {
    maxpixels <- 50000
  }

  rasterVis::gplot(map, maxpixels = maxpixels) +
    ggplot2::geom_tile(aes(fill = .data$value)) +
    ggplot2::scale_fill_gradientn(colours = colorramp,
                                  limits = c(0, 1),
                                  na.value = "transparent",
                                  name = lt) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(colour = "#666666"))
}
