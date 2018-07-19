#' Plot Presence Absence Map
#'
#' Plot a presence absence map using the given threshold.
#'
#' @param map raster. The \link{RaterLayer} object with the prediction.
#' @param th numeric. The threshold to convert the MaxEnt output in a presence/absence map.
#' @param colors vector. Colors to be used, default is NULL and uses red and blue.
#' @param hr logical, if TRUE produces an output with high resolution, default is FALSE.
#' @param filename character, if provided the raster map is saved in a file, default is NULL.
#' @param format character. The output format, see \link{writeRaster} for all the options,
#' default is Geotiff.
#' @param ... Additional parameters, see \link{writeRaster} for all the options.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plotPA(map, th = 7, colors = c("#7fbf7b", "#af8dc3"), filename = 'my_pa_map',
#' format = 'GTiff', overwrite = TRUE)}
#'
#' @author Sergio Vignali
plotPA <- function(map, th, colors = NULL, hr = FALSE, filename = NULL,
                   format = 'GTiff', ...) {

  if (class(map) != "RasterLayer")
    stop("Prediction must be a RasterLayer object!")

  pa <- map >= th

  if (!is.null(filename))
    writeRaster(pa, filename, format, ...)
  if (is.null(colors))
    colors <- c('#67a9cf', '#ef8a62')
  if (hr) {
    maxpixels = pa@ncols * pa@nrows
  } else {
    maxpixels = 50000
  }

  my_plot <- rasterVis::gplot(pa, maxpixels = maxpixels) +
    geom_tile(aes(fill = factor(value, labels = c("Absence", "Presence")))) +
    scale_fill_manual(values = colors,
                      breaks = c("Presence", "Absence"),
                      name = "") +
    coord_equal() +
    labs(title = "", x = "", y = "") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  return(my_plot)
}
