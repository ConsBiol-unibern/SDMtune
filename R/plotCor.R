#' Plot Correlation
#'
#' Plot a correlation matrix heat map with the value of the correlation
#' coefficients according with the given method. If cor_th is passed then it
#' prints only the coefficients that are higher or lower than the given
#' threshold.
#'
#' @details The code is inspired by the \href{www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization}{STHDA}
#' web page.
#'
#' @param bg SWD. The data frame used to compute the correlation matrix given as
#' MaxentSWD object, defaul NULL.
#' @param method character.  The method used to compute the correlation matrix,
#' default is "spearman".
#' @param cor_th numeric. If provided it prints only the coefficients that are
#' higher or lower than the given threshold, default is NULL.
#'
#' @return The plot object.
#' @export
#' @importFrom ggplot2 ggplot aes_ scale_fill_gradient2 theme_minimal theme
#' element_text coord_fixed element_blank geom_text
#' @importFrom reshape2 melt
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' my_plot <- plotCorrelation(mtcars, method = "spearman", cor_th = 0.7)}
#'
#' @author Sergio Vignali
plotCor <- function(bg, method = "spearman", cor_th = NULL) {

  if (class(bg) != "SWD") stop("Input must be a SWD object!")

  df <- bg@data

  # Remove categorical environmental variables
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)

  cor_matrix <- cor(df, method = method)
  cor_matrix[lower.tri(cor_matrix)] <- NA

  if (!is.null(cor_th)) {
    highly_correlated <- cor_matrix
    highly_correlated[abs(highly_correlated) < cor_th] <- NA
    highly_correlated <- reshape2::melt(highly_correlated, na.rm = TRUE)
  }

  cor_matrix <- reshape2::melt(cor_matrix, na.rm = TRUE)

  heat_map <- ggplot(data = cor_matrix, aes_(~Var2, ~Var1, fill = ~value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = paste0(toupper(substring(method, 1, 1)),
                                       substring(method, 2),
                                       "'s\ncoefficient")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12,
                                     hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(colour = "#666666", family = "sans-serif"))
    coord_fixed()

  if (is.null(cor_th)) {
    heat_map <- heat_map +
      geom_text(data = cor_matrix, aes_(~Var2, ~Var1,
                                        label = round(cor_matrix$value, 2)),
                color = "black", size = 3.5)
  } else {
    heat_map <- heat_map +
      geom_text(data = highly_correlated,
                aes_(~Var2, ~Var1, label = round(highly_correlated$value, 2)),
                color = "black", size = 3.5)
  }

  return(heat_map)
}
