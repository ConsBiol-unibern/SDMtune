#' Plot Response Curve
#'
#' Plot the Response Curve of the given environmental variable.
#'
#' @param model Maxent object.
#' @param variable character. Name of the variable to be plotted.
#' @param marginal logical, if TRUE it plots the marginal response courve, default is FALSE.
#' @param fun function used to compute the level of the other variables for marginal curves,
#' possible values are mean and median, default is mean.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param rug logical, if TRUE it adds the rug plot for presence and background locations locations,
#' available only for continuous variables, default is FALSE.
#' @param color The color of the curve, default is "red".
#'
#' @details Note that fun is not a character parameter, you must use mean and not "mean".
#'
#' @include Maxent_class.R
#' @import ggplot2
#' @importFrom raster modal
#'
#' @return The plot model
#' @export
#'
#' @examples
#' \dontrun{
#' plotResponse(model, variable = "bio12", marginal = T, fun = median, rug = T)}
#'
#' @author Sergio Vignali
plotResponse <- function(model, variable, marginal = FALSE, fun = mean,
                         clamp = TRUE, rug = FALSE, color = "red") {

  if (!variable %in% names(model@presence@data))
    stop(paste(variable, "is not used to train the model!"))

  set.seed(25)

  cont_vars <- names(Filter(is.numeric, model@presence@data))
  cat_vars <- names(Filter(is.factor, model@presence@data))

  if (variable %in% cat_vars) {
    categ <- unique(as.numeric(levels(bg@data[, variable]))[bg@data[, variable]])
    n_rows <- length(categ)
  } else {
    n_rows <- 100
  }

  train <- model@presence
  bg <- model@background
  train_rug <- data.frame(x = model@presence@data[, variable])
  bg_rug <- data.frame(x = model@background@data[, variable])
  data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(train@data)))
  colnames(data) <- colnames(train@data)
  data[cont_vars] <- apply(train@data[cont_vars], 2, fun)
  data[cat_vars] <- apply(train@data[cat_vars], 2, raster::modal)
  data <- do.call("rbind", replicate(n_rows, data, simplify = FALSE))

  if (clamp & variable %in% cont_vars) {
    var_min <- min(bg@data[variable])
    var_max <- max(bg@data[variable])
    train_rug$x <- train_rug[train_rug$x >= var_min & train_rug$x <= var_max, ]
  } else if (variable %in% cont_vars) {
    var_min <- min(rbind(train@data[variable], bg@data[variable]))
    var_max <- max(rbind(train@data[variable], bg@data[variable]))
  }

  if (variable %in% cont_vars) {
    data[variable] <- seq(var_min, var_max, length.out = n_rows)
  } else {
    data[variable] <- categ
  }

  if (!marginal) {
    train@data <- model@presence@data[variable]
    bg@data <- model@background@data[variable]
    model <- trainMaxent(train, bg, rm = model@rm, fc = model@fc,
                          type = model@type, iter = model@iter)
  }

  pred <- predict(model, data, clamp = clamp)
  plot_data <- data.frame(x = data[, variable], y = pred)

  if (variable %in% cont_vars) {
    my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
      geom_line(colour = color)

  } else {
    my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
      geom_bar(stat = "identity", fill = color) +
      scale_x_continuous(breaks = seq(min(plot_data$x),
                                      max(plot_data$x), 1))
  }

  my_plot <- my_plot +
    ggtitle(model@presence@species) +
    xlab(variable) +
    ylab(paste(model@type, "output")) +
    theme(plot.title = element_text(hjust = 0.5, face = "italic"),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  if (rug == TRUE & variable %in% cont_vars) {
    my_plot <- my_plot +
      geom_rug(data = train_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "t", color = "#4C4C4C") +
      geom_rug(data = bg_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "b", color = "#4C4C4C")
  } else if (rug == TRUE) {
    message("Warning: rug is available only for continous variables!")
  }

  gc()

  return(my_plot)
}
