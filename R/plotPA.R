#' Plot Presence Absence Map
#'
#' Plot a presence absence map using the given threshold.
#'
#' @param map \link[terra]{rast} object with the prediction.
#' @param th numeric. The threshold used to convert the output in a
#' presence/absence map.
#' @param colors vector. Colors to be used, default is `NULL` and it uses red
#' and blue.
#' @param hr logical. If `TRUE` it produces an output with high resolution.
#' @param filename character. If provided the raster map is saved in a file. It
#' must include the extension.
#' @param overwrite logical. If `TRUE` an existing file is overwritten.
#' @param wopt list. Writing options passed to \link[terra]{writeRaster}.
#' @param format character. Deprecated.
#' @param ... Unused arguments.
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
#' map <- terra::rast(matrix(runif(400, 0, 1),
#'                           nrow = 20,
#'                           ncol = 20))
#' plotPA(map,
#'        th = 0.8)
#'
#' # Custom colors
#' plotPA(map,
#'        th = 0.5,
#'        colors = c("#d8b365", "#018571"))
#'
#' # Save the file
#' \dontrun{
#' # The following command will save the map in the working directory. Note that
#' # the filename must include the extension
#' plotPA(map,
#'        th = 0.7,
#'        filename = "my_map.tif")
#' }
#' }
plotPA <- function(map,
                   th,
                   colors = NULL,
                   hr = FALSE,
                   filename = "",
                   overwrite = FALSE,
                   wopt = list(),
                   format = "",
                   ...) {

  if (!requireNamespace("rasterVis", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg rasterVis} to use this function",
      call = NULL
    )
  }

  # TODO: Remove with version 2.0.0
  if (inherits(map, "RasterLayer")) {
    .warn_raster("raster", "rast")
    map <- terra::rast(map)
  }

  if (!inherits(map, "SpatRaster"))
    cli::cli_abort(c(
      "!" = "{.var map} must be a {.cls SpatRaster} object",
      "x" = "You have supplied a {.cls {class(map)}} instead."
    ))

  # TODO: Remove with version 2.0.0
  if (format != "")
    cli::cli_warn(c(
      "!" = paste("The argument {.val format} is deprectated and will be",
                  "ignored. Use {.val wopt} instead and see {.val Details} in",
                  "{.fun terra::writeRaster}")
    ))

  pa <- map >= th

  if (filename != "") {
    file_ext <- tools::file_ext(filename)

    if (file_ext == "")
      cli::cli_abort(c(
        "x" = "Filename must include the extension"
      ))

    terra::writeRaster(pa,
                       filename = filename,
                       overwrite = overwrite,
                       wopt = wopt)
  }

  if (is.null(colors))
    colors <- c("#67a9cf", "#ef8a62")

  if (hr) {
    maxpixels <- terra::ncell(pa)
  } else {
    maxpixels <- 50000
  }

  my_plot <- rasterVis::gplot(pa, maxpixels = maxpixels)

  # In some cases rasterVis changes logical values into 0s and 1s
  my_plot$data$value <- as.logical(my_plot$data$value)

  my_plot +
    ggplot2::geom_tile(aes(fill = .data$value)) +
    ggplot2::scale_fill_manual(values = colors,
                               breaks = c(TRUE, FALSE),
                               labels = c("Presence", "Absence"),
                               name = "") +
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
