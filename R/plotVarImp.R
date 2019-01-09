#' Plot Varible Importance
#'
#' Plot the permutation variable importance as a bar plot.
#'
#' @param df data.frame. The result of varImp function.
#' @param color character. The color of the bar plot, default is grey.
#'
#' @return The ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes_ ylab scale_y_continuous geom_bar coord_flip
#' xlab theme_minimal
#' @importFrom scales percent
#'
#' @examples
#' \dontrun{
#' plotVarImp(my_table, color = '#a1d99b')}
#'
#' @author Sergio Vignali
plotVarImp <- function(df, color = "grey") {

  df <- df[order(df[, 2]), ]
  df[, 2] <- df[, 2] / 100
  df[, 1] <- factor(df[, 1], levels = df[, 1])
  my_plot <- ggplot(df, aes_(x = ~Variable, y = ~Permutation_importance)) +
    ylab("Permutation Importance") +
    xlab("") +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(position = "dodge", stat = "identity", fill = color) +
    coord_flip() +
    theme_minimal()

  return(my_plot)
}
