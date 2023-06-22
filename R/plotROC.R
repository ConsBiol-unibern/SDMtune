#' Plot ROC curve
#'
#' Plot the ROC curve of the given model and print the AUC value.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param test \linkS4class{SWD} object. The testing dataset.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          test = 0.2,
#'                          only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = train,
#'                fc = "l")
#'
#' # Plot the training ROC curve
#' plotROC(model)
#'
#' # Plot the training and testing  ROC curves
#' plotROC(model,
#'         test = test)
plotROC <- function(model,
                    test = NULL) {

  if (!requireNamespace("plotROC", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg plotROC} to use this function",
      call = NULL
    )
  }

  if (inherits(model@model, "Maxent")) {
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

  my_plot <- ggplot(df, aes(m = .data$pred, d = .data$pa, group = .data$set)) +
    plotROC::geom_roc(n.cuts = 0, aes(color = .data$set), size = 0.5) +
    ggplot2::scale_color_discrete(name = "AUC", labels = labels) +
    ggplot2::geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey",
                 linetype = 2) +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(colour = "#666666"))

  if (!is.null(test)) {
    my_plot <- my_plot +
      ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
  }

  my_plot
}
