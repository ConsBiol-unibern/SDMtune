#' Plot Correlation
#'
#' Plot a correlation matrix heat map with the value of the correlation
#' coefficients according with the given method. If cor_th is passed then it
#' prints only the coefficients that are higher or lower than the given
#' threshold.
#'
#' @param bg code{\linkS4class{SWD}} object used to compute the correlation
#' matrix.
#' @param method character. The method used to compute the correlation matrix,
#' default is "spearman".
#' @param cor_th numeric. If provided it prints only the coefficients that are
#' higher or lower than the given threshold, default is \code{NULL}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#' @importFrom ggplot2 ggplot aes_ scale_fill_gradient2 theme_minimal theme
#' element_text coord_fixed element_blank geom_text
#' @importFrom reshape2 melt
#' @importFrom stats cor
#' @importFrom stringr str_to_title
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
#' bg_coords <- dismo::randomPoints(predictors, 10000)
#'
#' # Create SWD object
#' bg <- prepareSWD(species = "Virtual species", a = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Plot heat map
#' plotCor(bg, method = "spearman")
#'
#' # Plot heat map showing only values higher than given threshold
#' plotCor(bg, method = "spearman", cor_th = 0.8)
#' }
plotCor <- function(bg, method = "spearman", cor_th = NULL) {

  cor_matrix <- corVar(bg, method = method, order = FALSE,
                       remove_diagonal = FALSE)
  label <- paste0(stringr::str_to_title(method), "'s\ncoefficient")

  heat_map <- ggplot(data = cor_matrix, aes_(~Var2, ~Var1, fill = ~value)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c",
                         limit = c(-1, 1), name = label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10,
                                     hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(colour = "#666666")) +
    coord_fixed()

  if (is.null(cor_th)) {
    heat_map <- heat_map +
      geom_text(data = cor_matrix, aes_(~Var2, ~Var1,
                                        label = round(cor_matrix$value, 2)),
                color = "black", size = 3)
  } else {
    highly_correlated <- corVar(bg, method = method, cor_th = cor_th,
                                order = FALSE, remove_diagonal = FALSE)
    heat_map <- heat_map +
      geom_text(data = highly_correlated,
                aes_(~Var2, ~Var1, label = round(highly_correlated$value, 2)),
                color = "black", size = 3)
  }

  return(heat_map)
}
