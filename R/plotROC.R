#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model SDMmodel or SDMmodelCV object.
#' @param val SWD the validation dataset, used only for SDMmodel object, default
#' is NULL.
#' @param test SWD the test dataset, used only for SDMmodel object, default is
#' NULL.
#'
#' @return The plot object.
#' @export
#' @import ggplot2
#' @importFrom pROC roc var
#'
#' @examples
#' \dontrun{
#' plotROC(model, test)}
#'
#' @author Sergio Vignali
plotROC <- function(model, val = NULL, test = NULL) {

  if (class(model) == "SDMmodel") {
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
      scale_colour_discrete(name = "AUC", labels = labels)
  } else {
    nf <- length(model@models)
    df <- data.frame()
    FPRs <- seq(0, 1, length.out = 1000)
    TPRs <- data.frame(tpr_1 = numeric(1000))
    aucs <- c()
    sds <- c()

    for (i in 1:nf) {
      cm <- confMatrix(model@models[[i]])
      fpr <- c(0, cm$fp / (cm$fp + cm$tn), 1)
      tpr <- c(0, cm$tp / (cm$tp + cm$fn), 1)
      df <- rbind(df, data.frame(fold = as.factor(i), fpr = fpr, tpr = tpr))
      aucs <- c(aucs, auc(model@models[[i]]))
      TPRs[paste0('tpr_', i)] <- approx(fpr, tpr, xout = FPRs)$y
      # Compute sd test AUC using Delong formula
      p <- predict(model@models[[i]], model@models[[i]]@presence,
                   type = "logistic")
      a <- predict(model@models[[i]], model@models[[i]]@background, type =
                     "logistic")
      predictions <- c(p, a)
      labs <- c(rep(1, length(p)), rep(0, length(a)))
      pred <- pROC::roc(labs, predictions)
      sds <- c(sds, sqrt(pROC::var(pred, method = "delong")))
    }

    meanTPR <- rowMeans(TPRs)
    meanTPR[1] <- 0
    sdTPR <- apply(TPRs, 1, sd)
    mean_data <- data.frame(x = FPRs, y = meanTPR, sd = sdTPR)
    mean_data$y_min <- apply(mean_data, 1,
                             function(x) max((x["y"] - x["sd"]), 0))
    mean_data$y_max <- apply(mean_data, 1,
                             function(x) min((x["y"] + x["sd"]), 1))
    meanAUC <- round(mean(aucs), 3)
    meanSD <- round(mean(sds), 3)

    my_plot <- ggplot(data = mean_data) +
      geom_line(aes_(x = ~x, y = ~y, colour = "r")) +
      geom_ribbon(aes_string(x = "x", ymin = "y_min", ymax = "y_max"),
                  fill = "red", alpha = 0.2) +
      scale_colour_manual(name = "AUC", values = c("r" = "red"),
                          labels = c(paste("Mean", meanAUC, "\n\u00B1",
                                           meanSD)))
  }

  my_plot <- my_plot +
    geom_segment(aes_(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    coord_fixed() +
    theme_minimal()

  return(my_plot)
}
