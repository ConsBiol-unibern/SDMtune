#' Plot Correlation
#'
#' Plot a correlation matrix heat map with the value of the correlation
#' coefficients according with the given method. If cor_th is passed then it
#' prints only the coefficients that are higher or lower than the given
#' threshold.
#'
#' @param bg \linkS4class{SWD} object used to compute the correlation matrix.
#' @param method character. The method used to compute the correlation matrix.
#' @param cor_th numeric. If provided it prints only the coefficients that are
#' higher or lower than the given threshold.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare background locations
#' bg_coords <- terra::spatSample(predictors,
#'                                size = 9000,
#'                                method = "random",
#'                                na.rm = TRUE,
#'                                xy = TRUE,
#'                                values = FALSE)
#'
#' # Create SWD object
#' bg <- prepareSWD(species = "Virtual species",
#'                  a = bg_coords,
#'                  env = predictors,
#'                  categorical = "biome")
#'
#' # Plot heat map
#' plotCor(bg,
#'         method = "spearman")
#'
#' # Plot heat map showing only values higher than given threshold
#' plotCor(bg,
#'         method = "spearman",
#'         cor_th = 0.8)
#' }
plotCor <- function(bg,
                    method = "spearman",
                    cor_th = NULL) {

  cor_matrix <- corVar(bg, method = method, order = FALSE,
                       remove_diagonal = FALSE)
  label <- paste0(stringr::str_to_title(method), "'s\ncoefficient")

  heat_map <- ggplot(data = cor_matrix, aes(x = .data$Var2, y = .data$Var1,
                                            fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf",
                                  high = "#d7191c", limit = c(-1, 1),
                                  name = label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       size = 10, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 10),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   text = ggplot2::element_text(colour = "#666666")) +
    ggplot2::coord_fixed()

  if (is.null(cor_th)) {
    heat_map <- heat_map +
      ggplot2::geom_text(data = cor_matrix,
                         aes(x = .data$Var2, y = .data$Var1,
                             label = round(.data$value, 2)),
                         color = "black", size = 3)
  } else {
    highly_correlated <- corVar(bg, method = method, cor_th = cor_th,
                                order = FALSE, remove_diagonal = FALSE)
    heat_map <- heat_map +
      ggplot2::geom_text(data = highly_correlated,
                         aes(x = .data$Var2, y = .data$Var1,
                             label = round(.data$value, 2)),
                         color = "black", size = 3)
  }

  heat_map
}
