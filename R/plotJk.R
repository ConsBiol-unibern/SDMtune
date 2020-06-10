#' Plot Jackknife Test
#'
#' Plot the Jackknife Test for variable importance.
#'
#' @param jk data.frame with the output of the \link{doJk} function.
#' @param type character, "train" or "test" to plot the result of the test on
#' the train or testing dataset.
#' @param ref numeric. The value of the chosen metric for the model trained
#' using all the variables. If provided it plots a vertical line showing the
#' reference value. Default is `NULL`.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "lq")
#'
#' # Execute the Jackknife test for all the environmental variables using the
#' # metric AUC
#' jk <- doJk(model, metric = "auc", test = test)
#'
#' # Plot Jackknife test result for training
#' plotJk(jk, type = "train", ref = auc(model))
#'
#' #' # Plot Jackknife test result for testing
#' plotJk(jk, type = "test", ref = auc(model, test = test))
#' }
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
    metric <- "aicc"
    y_label <- "AICc"
  }

  if (type == "test" & metric == "aicc")
    stop("Test mode is not available with aicc!")

  if (metric != "aicc") {
    if (type == "train") {
      with <- 3
      without <- 2
    } else {
      with <- 5
      without <- 4
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
                    rep("Without", nrow(df_without)))
  my_plot <- ggplot(df_plot, aes(x = .data$Variable, y = .data$value,
                                 fill = .data$test)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(colour = "#666666"),
                   legend.title = ggplot2::element_blank())


  if (!is.null(ref))
    my_plot <- my_plot +
    ggplot2::geom_hline(yintercept = ref, linetype = "dashed", color = "red")

  return(my_plot)
}
