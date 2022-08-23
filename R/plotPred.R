#' Plot Prediction
#'
#' Plot Prediction output.
#'
#' @param map \link[raster]{raster} object with the prediction.
#' @param lt character. Legend title, default is an empty string.
#' @param colorramp vector. A custom color ramp given as a vector of colors
#' (see example), default is `NULL` and uses a blue/red color ramp.
#' @param hr logical, if `TRUE` produces an output with high resolution,
#' default is `FALSE`.
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
#' map <- raster::raster(matrix(runif(400, 0, 1), 20, 20))
#' plotPred(map, lt = "Habitat suitability \ncloglog")
#' # Custom colors
#' plotPred(map, lt = "Habitat suitability",
#'          colorramp = c("#2c7bb6", "#ffffbf", "#d7191c"))
#' }
plotPred <- function(map, lt = "", colorramp = NULL, hr = FALSE) {

  if (!requireNamespace("rasterVis", quietly = TRUE)) {
    stop("You need the packege \"rasterVis\" to run this function,",
         " please install it.",
         call. = FALSE)
  }

  if (!inherits(map, "RasterLayer"))
    stop("Prediction must be a RasterLayer object!")
  if (is.null(colorramp))
    colorramp <- c("blue", "cyan", "green", "yellow", "red")

  if (hr) {
    maxpixels <- map@ncols * map@nrows
  } else {
    maxpixels <- 50000
  }

  my_plot <- rasterVis::gplot(map, maxpixels = maxpixels) +
    ggplot2::geom_tile(aes(fill = .data$value)) +
    ggplot2::scale_fill_gradientn(colours = colorramp, limits = c(0, 1),
                                  na.value = "transparent", name = lt) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(colour = "#666666"))

  return(my_plot)
}
