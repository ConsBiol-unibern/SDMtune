#' Plot Varible Importance
#'
#' Plot the variable importance as a bar plot.
#'
#' @param model Maxent object.
#' @param color character. The color of the bar plot, default is grey.
#' @param type character, contribution or permutation (or an abbreviation of them) are possible values.
#'
#' @return The ggplot object.
#' @export
#' @import ggplot2
#' @importFrom scales percent
#'
#' @examples
#' \dontrun{
#' plotContribution(model, type = "permut", color = '#a1d99b')}
#'
#' @author Sergio Vignali
plotVarImp <- function(model, type = c("contribution", "permutation"),
                       color = "grey") {

  if (class(model) != "Maxent") stop("Model must be a Maxent object!")

  type <- match.arg(type)
  df <- varImp(model)

  if (type == "contribution") {
    df <- df[nrow(df):1, ]
    df$Percent_contribution <- df$Percent_contribution / 100
    df$Variable <- factor(df$Variable, levels = df$Variable)
    my_plot <- ggplot(df, aes_(x = ~Variable, y = ~Percent_contribution)) +
      ylab("Percent Contribution")
  } else {
    df <- df[order(df$Permutation_importance), ]
    df$Permutation_importance <- df$Permutation_importance / 100
    df$Variable <- factor(df$Variable, levels = df$Variable)
    my_plot <- ggplot(df, aes_(x = ~Variable, y = ~Permutation_importance)) +
      ylab("Permutation Importance")
  }
  my_plot <- my_plot +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(position = "dodge", stat = "identity", fill = color) +
    coord_flip() +
    ggtitle(model@presence@species) +
    xlab("") +
    theme(plot.title = element_text(hjust = 0.5, face = "italic"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  return(my_plot)
}
