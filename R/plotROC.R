#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model Maxen object.
#' @param val SWD the validation dataset, default is NULL.
#' @param test SWD the test dataset, default is NULL.
#'
#' @return The plot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plotROC(model, test)}
#'
#' @author Sergio Vignali
plotROC <- function(model, val = NULL, test = NULL) {
  if (class(model) != "Maxent")
    stop("Model must be a Maxent object!")

  cm <- confMatrix(model)
  fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
  tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
  df <- data.frame(set = "train", fpr = fpr, tpr = tpr)
  labels <- c(paste("Train", model@results["Training.AUC", ]))

  if (!is.null(val)) {
    cm <- confMatrix(model, val)
    fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
    tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
    auc <- auc(model, presence = val)
    df_val <- data.frame(set = "val", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_val)
    labels <- append(labels, paste("Val", round(auc, 3)))
  }

  if (!is.null(test)) {
    cm <- confMatrix(model, test)
    fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
    tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
    auc <- auc(model, presence = test)
    df_test <- data.frame(set = "test", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_test)
    labels <- append(labels, paste("Test", round(auc, 3)))
  }

  my_plot <- ggplot(df, aes(x = fpr, y = tpr, colour = set)) +
    geom_line() +
    scale_colour_discrete(name = "AUC", labels = labels) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    coord_fixed() +
    theme(legend.justification = c(1, 0),
          legend.position = c(1, 0),
          legend.box.margin = margin(c(10,10,10,10)),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  return(my_plot)
}
