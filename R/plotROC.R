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

  train <- model@presence@data
  bg <- model@background@data
  variables <- colnames(train)
  maxent_model <- Maxent2MaxEnt(model)
  train_evaluation <- dismo::evaluate(maxent_model, p = train, a = bg)
  df <- data.frame(set = "train", FPR = train_evaluation@FPR,
                   TPR = train_evaluation@TPR)
  labels <- c(paste("Train", sprintf("%.3f", round(train_evaluation@auc, 3))))

  if (!is.null(val)) {
    val_evaluation <- dismo::evaluate(maxent_model, p = val@data[variables], a = bg)
    df_val <- data.frame(set = "val", FPR = val_evaluation@FPR,
                         TPR = val_evaluation@TPR)
    df <- rbind(df, df_val)
    labels <- append(labels,
                     paste("Val", sprintf("%.3f", round(val_evaluation@auc, 3))))
  }

  if (!is.null(test)) {
    test_evaluation <- dismo::evaluate(maxent_model, p = test@data[variables], a = bg)
    df_test <- data.frame(set = "test", FPR = test_evaluation@FPR,
                          TPR = test_evaluation@TPR)
    df <- rbind(df, df_test)
    labels <- append(labels,
                     paste("Test", sprintf("%.3f", round(test_evaluation@auc, 3))))
  }

  my_plot <- ggplot(df, aes(x = FPR, y = TPR, colour = set)) +
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
