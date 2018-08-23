#' Plot Jackknife Test
#'
#' Plot the Jackknife Test for variable importance.
#'
#' @param jk data.frame with the output of the **doJk** function.
#' @param type character, "train" or "test" to plot the result of the test on
#' the train or test dataset.
#' @param ref numeric. The value of the chosen metric for the model trained
#' using all the variables. If provided it plots a vertical line showing the
#' reference value. Default is NULL.
#'
#' @return The ggplot object.
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plotJk(jk_test, type = "train", ref = auc(my_model))}
#'
#' @author Sergio Vignali
plotJk <- function(jk, type = c("train", "test"), ref = NULL) {

  if (!is.data.frame(jk))
    jk <- jk$results

  type <- match.arg(type)

  if (grepl("AUC", colnames(jk[2]))) {
    metric <- "auc"
    y_label <- paste(stringr::str_to_title(type), "AUC")
  } else if (grepl("TSS", colnames(jk[2]))) {
    metric <- "tss"
    y_label <- paste(stringr::str_to_title(type), "TSS")
  } else {
    metric = "aicc"
    y_label <- "AICc"
  }

  if (type == "test") {
    if (metric == "aicc")
      stop("Test mode is not available with aicc!")
    if (!grepl("Test", paste(colnames(jk), collapse = "")))
      stop("Jackknife data frame doesn't have Test column!")
  }

  if (metric != "aicc") {
    if (type == "train") {
      with <- 3
      without <- 2
    } else {
      if (ncol(jk) == 5) {
        with <- 5
        without <- 4
      } else {
        with <- NULL
        without <- 3
      }
    }
  } else {
    with <- 3
    without <- 2
  }

  jk$Variable <- factor(jk$Variable, levels = jk$Variable)

  df_without <- jk[c(1, without)]
  if (grepl("withonly", paste(colnames(jk), collapse = ""))) {
    df_with <- jk[c(1, with)]
  } else {
    df_with <- data.frame(Variable = character(), value = numeric())
  }

  names(df_with) <- names(df_without) <- c("Variable", "value")
  df_plot <- rbind(df_with, df_without)
  df_plot$test <- c(rep("With only", nrow(df_with)),
                    rep("Whitout", nrow(df_without)))
  my_plot <- ggplot(df_plot, aes_(x = ~Variable, y = ~value, fill = ~test)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    coord_flip() +
    xlab("") +
    ylab(y_label) +
    theme_minimal()

  if (!is.null(ref))
    my_plot <- my_plot +
      geom_hline(yintercept = ref, linetype = "dashed", color = "red")

  return(my_plot)
}
