#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model \code{\link{SDMmodel}} object.
#' @param test \code{\link{SWD}} object. The testing dataset, default is
#' \code{NULL}.
#'
#' @return The plot object.
#' @export
#' @importFrom ggplot2 ggplot aes_ geom_segment labs coord_fixed theme_minimal
#' theme scale_color_discrete guides guide_legend
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
plotROC <- function(model, test = NULL) {

  if (!requireNamespace("plotROC", quietly = TRUE)) {
    stop("You need the packege \"plotROC\" to run this function,",
         " please install it.",
         call. = FALSE)
  }

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  df <- data.frame(set = "Train", pa = model@data@pa,
                   pred = predict(model, data = model@data, type = type),
                   stringsAsFactors = FALSE)
  auc <- auc(model)
  labels <- paste("Train", round(auc, 3))

  if (!is.null(test)) {
    df_test <- data.frame(set = "Test", pa = test@pa,
                          pred = predict(model, data = test, type = type),
                          stringsAsFactors = FALSE)
    df <- rbind(df, df_test)
    auc <- auc(model, test = test)
    labels <- c(paste("Test", round(auc, 3)), labels)
  }

  my_plot <- ggplot(df, aes_(m = ~pred, d = ~pa, group = ~set)) +
    plotROC::geom_roc(n.cuts = 0, aes_(color = ~set), size = 0.5) +
    scale_color_discrete(name = "AUC", labels = labels) +
    geom_segment(aes_(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    labs(x = "False Positive Rate", y = "True Positive Rate") +
    coord_fixed() +
    theme_minimal() +
    theme(text = element_text(colour = "#666666"))

  if (!is.null(test)) {
    my_plot <- my_plot + guides(colour = guide_legend(reverse = TRUE))
  }

  return(my_plot)
}
