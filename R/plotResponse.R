#' Plot Response Curve
#'
#' Plot the Response Curve of the given environmental variable.
#'
#' @param model Maxent object.
#' @param var character. Name of the variable to be plotted.
#' @param type character. Output type, see \link{predict,Maxent-method} for
#' Maxent models or \link{predict.maxnet} for Maxnet models.
#' @param marginal logical, if TRUE it plots the marginal response courve,
#' default is FALSE.
#' @param fun function used to compute the level of the other variables for
#' marginal curves, possible values are mean and median, default is mean.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param rug logical, if TRUE it adds the rug plot for presence and background
#' locations locations, available only for continuous variables, default is
#' FALSE.
#' @param color The color of the curve, default is "red".
#'
#' @details Note that fun is not a character parameter, you must use mean and
#' not "mean".
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
#' plotResponse(model, var = "bio12", marginal = T, fun = median, rug = T)}
#'
#' @author Sergio Vignali
plotResponse <- function(model, var, type, marginal = FALSE, fun = mean,
                         clamp = TRUE, rug = FALSE, color = "red") {

  if (!var %in% names(model@presence@data))
    stop(paste(var, "is not used to train the model!"))

  set.seed(25)

  train <- model@presence
  bg <- model@background
  cont_vars <- names(Filter(is.numeric, train@data))
  cat_vars <- names(Filter(is.factor, train@data))

  if (var %in% cat_vars) {
    categ <- unique(as.numeric(levels(bg@data[, var]))[bg@data[, var]])
    n_rows <- length(categ)
  } else {
    n_rows <- 100
  }

  train_rug <- data.frame(x = train@data[, var])
  bg_rug <- data.frame(x = bg@data[, var])
  data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(train@data)))
  colnames(data) <- colnames(train@data)
  data[cont_vars] <- apply(train@data[cont_vars], 2, fun)
  data[cat_vars] <- apply(train@data[cat_vars], 2, raster::modal)
  data <- do.call("rbind", replicate(n_rows, data, simplify = FALSE))

  if (clamp & var %in% cont_vars) {
    var_min <- min(bg@data[var])
    var_max <- max(bg@data[var])
    train_rug <- data.frame(x = train_rug[train_rug$x >= var_min
                                          & train_rug$x <= var_max, ])
  } else if (var %in% cont_vars) {
    var_min <- min(rbind(train@data[var], bg@data[var]))
    var_max <- max(rbind(train@data[var], bg@data[var]))
  }

  if (var %in% cont_vars) {
    data[var] <- seq(var_min, var_max, length.out = n_rows)
  } else {
    data[var] <- categ
  }

  if (!marginal) {
    train@data <- model@presence@data[var]
    bg@data <- model@background@data[var]
    method <- class(model@model)

    if (method == "Maxent") {
      iter <- model@model@iter
      extra_args <- model@model@extra_args
    } else {
      iter <- NULL
      extra_args <- NULL
    }

    model <- train(method = method, presence = train, bg = bg,
                   reg = model@model@reg, fc = model@model@fc, iter = iter,
                   extra_args = extra_args)
  }

  pred <- predict(model, data, type = type, clamp = clamp)
  plot_data <- data.frame(x = data[, var], y = pred)

  if (var %in% cont_vars) {
    my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
      geom_line(colour = color)

  } else {
    my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
      geom_bar(stat = "identity", fill = color) +
      scale_x_continuous(breaks = seq(min(plot_data$x),
                                      max(plot_data$x), 1))
  }

  my_plot <- my_plot +
    xlab(var) +
    ylab(paste(type, "output")) +
    theme_minimal()

  if (rug == TRUE & var %in% cont_vars) {
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
