#' Plot Variable Importance
#'
#' Plot the variable importance as a bar plot.
#'
#' @param df data.frame. A data.frame containing the the name of the variables
#' as first column and the value of the variable importance as second column.
#' @param color character. The color of the bar plot, default is grey.
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
#' # Train a model
#' model <- train(method = "Maxnet", data = data, fc = "l")
#'
#' # Compute variable importance
#' vi <- varImp(model, permut = 1)
#'
#' # Plot variable importance
#' plotVarImp(vi)
#'
#' # Plot variable importance with custom color
#' plotVarImp(vi, color = "red")
plotVarImp <- function(df, color = "grey") {

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("You need the packege \"scales\" to run this function,",
         " please install it.",
         call. = FALSE)
  }

  df <- df[order(df[, 2]), ]
  df[, 2] <- df[, 2] / 100
  df[, 1] <- factor(df[, 1], levels = df[, 1])
  y_name <- colnames(df)[2]
  my_plot <- ggplot(df, aes(x = .data$Variable, y = .data[[y_name]])) +
    ggplot2::labs(x = "", y = sub("_", " ", y_name)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_bar(position = "dodge", stat = "identity", fill = color) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(colour = "#666666"))

  return(my_plot)
}
