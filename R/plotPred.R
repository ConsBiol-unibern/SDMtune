#' Plot Prediction
#'
#' Plot Prediction output.
#'
#' @param map raster. The \link{raster} object with the prediction.
#' @param lt character. Legend title, default is an empty string.
#' @param colorramp vector. A custom color ramp given as a vector of colors (see example),
#' default is NULL and uses a colorramp similar to the original MaxEnt output.
#' See \href{color brewer}{http://colorbrewer2.org/} to build nice colorramp.
#' @param hr logical, if TRUE produces an output with high resolution, default is FALSE.
#'
#' @return A ggplot object.
#' @export
#' @importFrom rasterVis gplot
#'
#' @examples
#' \dontrun{
#' plotPrediction(my_map, colorramp = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))}
#'
#' @author Sergio Vignali
plotPred <- function(map, lt = "", colorramp = NULL, hr = FALSE) {

  if (class(map) != "RasterLayer")
    stop("Prediction must be a RasterLayer object!")
  if (is.null(colorramp))
    colorramp <- c("blue", "cyan", "green", "yellow", "red")

  if (hr) {
    maxpixels = map@ncols * map@nrows
  } else {
    maxpixels = 50000
  }

  my_plot <- rasterVis::gplot(map, maxpixels = maxpixels) +
    geom_tile(aes_(fill = ~value)) +
    scale_fill_gradientn(colours = colorramp,
                         limits = c(0, 1),
                         na.value = "transparent",
                         name = lt) +
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
