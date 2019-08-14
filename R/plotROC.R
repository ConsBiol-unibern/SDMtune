#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model \code{\link{SDMmodel}} object.
#' @param test \code{\link{SWD}} object. The testing dataset, default is
#' \code{NULL}.
#' @param val deprecated.
#'
#' @return The plot object.
#' @export
#' @importFrom ggplot2 ggplot aes_ geom_line scale_colour_discrete geom_segment
#' labs coord_fixed theme_minimal theme
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
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Plot the training ROC curve
#' plotROC(model)
#'
#' # Plot the training and testing  ROC curves
#' plotROC(model, test = test)
#' }
#'
#' @author Sergio Vignali
plotROC <- function(model, test = NULL, val = NULL) {

  # TODO Remove it next release
  if (!is.null(val)) {
    stop("\"val\" argument is deprecated and will be removed in the next ",
         "release.", call. = FALSE)
  }

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  cm <- confMatrix(model, type = type)
  fpr <- cm$fp / (cm$fp + cm$tn)
  tpr <- cm$tp / (cm$tp + cm$fn)
  auc <- auc(model)
  df <- data.frame(set = "Train", fpr = fpr, tpr = tpr,
                   stringsAsFactors = FALSE)

  if (!is.null(test)) {
    cm <- confMatrix(model, type = type, test = test)
    fpr <- cm$fp / (cm$fp + cm$tn)
    tpr <- cm$tp / (cm$tp + cm$fn)
    auc <- auc(model, test = test)
    df_test <- data.frame(set = "Test", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_test)
  }

  my_plot <- ggplot(df, aes_(x = ~fpr, y = ~tpr, group = ~set)) +
    geom_line(aes_(color = ~set)) +
    geom_segment(aes_(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    labs(x = "False Positive Rate", y = "True Positive Rate", color = "AUC") +
    coord_fixed() +
    theme_minimal() +
    theme(text = element_text(colour = "#666666", family = "sans-serif"))

  return(my_plot)
}
