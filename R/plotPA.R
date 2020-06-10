#' Plot Presence Absence Map
#'
#' Plot a presence absence map using the given threshold.
#'
#' @param map \link[raster]{raster} object with the prediction.
#' @param th numeric. The threshold used to convert the output in a
#' presence/absence map.
#' @param colors vector. Colors to be used, default is `NULL` and uses red and
#' blue.
#' @param hr logical, if `TRUE` produces an output with high resolution,
#' default is `FALSE`.
#' @param filename character, if provided the raster map is saved in a file,
#' default is `NULL`.
#' @param format character. The output format, see \link[raster]{writeRaster}
#' for all the options, default is Geotiff.
#' @param ... Additional arguments, see \link[raster]{writeRaster} for all the
#' options.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @seealso \link{plotPred}.
#'
#' @examples
#' \donttest{
#' map <- raster::raster(matrix(runif(400, 0, 1), 20, 20))
#' plotPA(map, th = 0.8)
#' # Custom colors
#' plotPA(map, th = 0.5, colors = c("#d8b365", "#018571"))
#' # Save the file
#' \dontrun{
#' # The following command will save the map in the working directory
#' plotPA(map, th = 0.7, filename = "my_map", format = "ascii")
#' }
#' }
plotPA <- function(map, th, colors = NULL, hr = FALSE, filename = NULL,
                   format = "GTiff", ...) {

  if (!requireNamespace("rasterVis", quietly = TRUE)) {
    stop("You need the packege \"rasterVis\" to run this function,",
         " please install it.",
         call. = FALSE)
  }

  if (class(map) != "RasterLayer")
    stop("Prediction must be a RasterLayer object!")

  pa <- map >= th

  if (!is.null(filename))
    raster::writeRaster(pa, filename, format, ...)
  if (is.null(colors))
    colors <- c("#67a9cf", "#ef8a62")
  if (hr) {
    maxpixels <- pa@ncols * pa@nrows
  } else {
    maxpixels <- 50000
  }

  my_plot <- rasterVis::gplot(pa, maxpixels = maxpixels)

  # In some cases rasterVis changes logical values into 0s and 1s
  my_plot$data$value <- as.logical(my_plot$data$value)

  my_plot <- my_plot +
    ggplot2::geom_tile(aes(fill = .data$value)) +
    ggplot2::scale_fill_manual(values = colors, breaks = c(TRUE, FALSE),
                               labels = c("Presence", "Absence"), name = "") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2:: theme(plot.title = ggplot2::element_text(hjust = 0.5),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    text = ggplot2::element_text(colour = "#666666"))

  return(my_plot)
}
