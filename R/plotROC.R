#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model \link{SDMmodel} object.
#' @param val \link{SWD} object. The validation dataset, default is NULL.
#' @param test \link{SWD} object. The testing dataset, default is NULL.
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
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' bg <- prepareSWD(species = "Vultur gryphus", coords = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "l")
#'
#' # Plot the training ROC curve
#' plotROC(model)
#'
#' # Plot the training and testing  ROC curves
#' plotROC(model, test = test)
#' }
#'
#' @author Sergio Vignali
plotROC <- function(model, val = NULL, test = NULL) {

  cm <- confMatrix(model)
  fpr <- cm$fp / (cm$fp + cm$tn)
  tpr <- cm$tp / (cm$tp + cm$fn)
  auc <- auc(model)
  df <- data.frame(set = "train", fpr = fpr, tpr = tpr,
                   stringsAsFactors = FALSE)
  labels <- c(paste("Train", round(auc, 3)))

  if (!is.null(val)) {
    cm <- confMatrix(model, test = val)
    fpr <- cm$fp / (cm$fp + cm$tn)
    tpr <- cm$tp / (cm$tp + cm$fn)
    auc <- auc(model, test = val)
    df_val <- data.frame(set = "val", fpr = fpr, tpr = tpr)
    df <- rbind(df, df_val)
    labels <- append(labels, paste("Val", round(auc, 3)))
  }

  if (!is.null(test)) {
    cm <- confMatrix(model, test = test)
    fpr <- cm$fp / (cm$fp + cm$tn)
    tpr <- cm$tp / (cm$tp + cm$fn)
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
    labs(x = "False Positive Rate", y = "True Positive Rate") +
    coord_fixed() +
    theme_minimal() +
    theme(text = element_text(colour = "#666666", family = "sans-serif"))

  return(my_plot)
}
