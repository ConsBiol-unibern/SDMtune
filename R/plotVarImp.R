#' Plot Variable Importance
#'
#' Plot the variable importance as a bar plot.
#'
#' @param df data.frame. A data.frame containing the the name of the variables
#' as first column and the value of the variable importance as second column.
#' @param color character. The color of the bar plot, default is grey.
#'
#' @return The ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes_string ylab scale_y_continuous geom_bar
#' coord_flip labs theme_minimal theme element_text
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
  my_plot <- ggplot(df, aes_string(x = "Variable", y = colnames(df)[2])) +
    labs(x = "", y = sub("_", " ", colnames(df)[2])) +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(position = "dodge", stat = "identity", fill = color) +
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(colour = "#666666", family = "sans-serif"))

  return(my_plot)
}
