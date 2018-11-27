#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model SDMmodel.
#' @param val SWD the validation dataset, default is NULL.
#' @param test SWD the test dataset, default is NULL.
#'
#' @return The plot object.
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plotROC(model, test)}
#'
#' @author Sergio Vignali
plotROC <- function(model, val = NULL, test = NULL) {

  cm <- confMatrix(model)
  fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
  tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
  auc <- auc(model)
  df <- data.frame(set = "train", fpr = fpr, tpr = tpr)
  labels <- c(paste("Train", round(auc, 3)))

  if (!is.null(val)) {
    cm <- confMatrix(model, test = val)
    fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
    tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
    auc <- auc(model, test = val)
    df_val <- data.frame(set = "val", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_val)
    labels <- append(labels, paste("Val", round(auc, 3)))
  }

  if (!is.null(test)) {
    cm <- confMatrix(model, test = test)
    fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
    tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
    auc <- auc(model, test = test)
    df_test <- data.frame(set = "test", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_test)
    labels <- append(labels, paste("Test", round(auc, 3)))
  }

  my_plot <- ggplot(df, aes_(x = ~fpr, y = ~tpr, colour = ~set)) +
    geom_line() +
      scale_colour_discrete(name = "AUC", labels = labels) +
    geom_segment(aes_(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    coord_fixed() +
    theme_minimal()

  return(my_plot)
}
