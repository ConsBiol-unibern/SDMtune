#' Plot Jackknife Test
#'
#' Plot the Jackknife Test for variable importance.
#'
#' @param model Maxent object.
#' @param jk data.frame with the output of the doJk function.
#' @param type character, train or test to plot the train or test AUC values.
#'
#' @return The ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plotJk(model, jk_test = 'AUC')}
#'
#' @author Sergio Vignali
plotJk <- function(model, jk, type = c("train", "test")) {

  type <- match.arg(type)
  y_label <- paste(stringr::str_to_title(type), "AUC")
  with = paste0(stringr::str_to_title(type), "_AUC_withonly")
  without = paste0(stringr::str_to_title(type), "_AUC_without")

  if (type == "train") {
    model_value <- model@results["Training.AUC", ]
  } else {
    if (length(jk$Test_AUC_without) == 0)
      stop("Test AUC not present in the Jackknife data frame!")
    model_value <- model@results["Test.AUC", ]
  }

  jk$Variable <- factor(jk$Variable, levels = jk$Variable)

  df_without <- jk[c("Variable", without)]
  if (with %in% colnames(jk)) {
    df_with <- jk[c("Variable", with)]
  } else {
    df_with <- data.frame(Variable = character(), value = numeric())
  }

  names(df_with) <- names(df_without) <- c("Variable", "value")
  df_plot <- rbind(df_with, df_without)
  df_plot$test <- c(rep("With only", nrow(df_with)),
                    rep("Whitout", nrow(df_without)))
  my_plot <- ggplot(df_plot, aes(x = Variable, y = value, fill = test)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    coord_flip() +
    geom_hline(yintercept = model_value, linetype = "dashed", color = "red") +
    ggtitle("Jackknife Test") +
    xlab("") +
    ylab(y_label) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank())

  return(my_plot)
}
