#' Plot Prediction
#'
#' Plot Prediction output.
#'
#' @param map \code{\link[raster]{raster}} object with the prediction.
#' @param lt character. Legend title, default is an empty string.
#' @param colorramp vector. A custom color ramp given as a vector of colors
#' (see example), default is \code{NULL} and uses a color ramp similar to the
#' original MaxEnt output.
#' @param hr logical, if \code{TRUE} produces an output with high resolution,
#' default is \code{FALSE}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#' @importFrom ggplot2 geom_tile scale_fill_gradientn coord_equal labs
#' scale_x_continuous scale_y_continuous theme_minimal theme element_text
#' element_blank
#' @importFrom rasterVis gplot
#'
#' @author Sergio Vignali
#'
#' @seealso \code{\link{plotPA}}
#'
#' @examples
#' \donttest{
#' map <- raster::raster(matrix(runif(400, 0, 1), 20, 20))
#' plotPred(map, lt = "Habitat suitability \ncloglog")
#' # Custom colors
#' plotPred(map, lt = "Habitat suitability",
#'          colorramp = c("#2c7bb6", "#ffffbf", "#d7191c"))
#' }
plotPred <- function(map, lt = "", colorramp = NULL, hr = FALSE) {

  if (class(map) != "RasterLayer")
    stop("Prediction must be a RasterLayer object!")
  if (is.null(colorramp))
    colorramp <- c("blue", "cyan", "green", "yellow", "red")

  if (hr) {
    maxpixels <- map@ncols * map@nrows
  } else {
    maxpixels <- 50000
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
          axis.ticks.y = element_blank(),
          text = element_text(colour = "#666666"))

  return(my_plot)
}
