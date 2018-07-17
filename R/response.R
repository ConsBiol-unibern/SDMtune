#' Response Curve
#'
#' Plot the Responce Courve of the given environmental variable
#'
#' @param model A MaxentModel object.
#' @param variable A variable name, used to plot only one variable.
#' @param marginal Flag to plot the marginal response courves, default is FALSE.
#' @param rug Flag to add the rug plot for presence locations, available only for continuous variables,
#' default is FALSE.
#' @param color The color of the curve, default is "red".
#'
#' @return The plot objects
#' @export
#'
#' @examples
#' \dontrun{responce(model, variable = 'bio12', marginal = TRUE, rug = TRUE)}
#'
#' @author Sergio Vignali
response <- function(model, variable,
                     maxent_output = c("cloglog", "logistic", "raw"),
                     clamp = TRUE, marginal = FALSE, rug = FALSE,
                     color = "red") {
  if (class(model) != "Maxent")
    stop("Model must be a Maxent object!")

  if (!variable %in% names(model@presence@data))
    stop(paste(variable, "is not used to train the model!"))

  cont_vars <- names(Filter(is.numeric, model@presence@data))
  cat_vars <- names(Filter(is.factor, model@presence@data))
  maxent_output <- match.arg(maxent_output)

  train <- model@presence
  bg <- model@background
  train_rug <- data.frame(x = model@presence@data[, variable])
  bg_rug <- data.frame(x = model@background@data[, variable])

  if (marginal) {
    train@data <- model@presence@data[variable]
    bg@data <- model@background@data[variable]
    model <- trainMaxent(train, bg, rm = model@rm, fc = model@fc,
                         maxent_output = maxent_output,
                         iterations = model@iterations)

    data <- mergeSWD(train, bg)
    if (clamp & !variable %in% cat_vars ) {
      data <- data.frame(x = data@data[, 1])
      data <- subset(data, x >= model@min_max[1, ]$min &
                     x <= model@min_max[1, ]$max)
      colnames(data) <- variable
      train_rug <- subset(train_rug, x >= model@min_max[1, ]$min &
                          x <= model@min_max[1, ]$max)
      bg_rug <- subset(bg_rug, x >= model@min_max[1, ]$min &
                       x <= model@min_max[1, ]$max)
    } else {
      data <- data@data
      if (variable %in% cat_vars) {
        categ <- unique(as.numeric(levels(data[, 1]))[data[, 1]])
        data <- data.frame(x <- categ)
        colnames(data) <- variable
      }
    }
  } else {
    if (clamp) {
      for (var in model@min_max$variable) {
        data[var] <- raster::clamp(data[, var],
                                   model@min_max$min[model@min_max$variable == var],
                                   model@min_max$max[model@min_max$variable == var])
      }
      train_rug <- subset(train_rug, x >= model@min_max[1,]$min &
                            x <= model@min_max[1,]$max)
      bg_rug <- subset(bg_rug, x >= model@min_max[1,]$min &
                         x <= model@min_max[1,]$max)
    }

  }

  pred <- suppressMessages(predict(model, data, maxent_output = maxent_output,
                                   clamp = FALSE))
  plot_data <- data.frame(x = data[, variable], y = pred)

  if (variable %in% cont_vars) {
    my_plot <- ggplot() +
      geom_line(data = plot_data, aes(x = x, y = y), colour = color)

  } else {
    my_plot <- ggplot(data = plot_data, aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = color) +
      scale_x_continuous(breaks = seq(min(plot_data$x), max(plot_data$x), 1))
  }

  my_plot <- my_plot +
    ggtitle(model@presence@species) +
    xlab(variable) +
    ylab(paste(maxent_output, "output")) +
    theme(plot.title = element_text(hjust = 0.5, face = "italic"),
          legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  if (rug == TRUE & variable %in% cont_vars) {
    my_plot <- my_plot +
      geom_rug(data = train_rug, inherit.aes = FALSE, aes(x),
               sides = "t", color = "#4C4C4C") +
      geom_rug(data = bg_rug, inherit.aes = FALSE, aes(x),
               sides = "b", color = "#4C4C4C")
  } else if (rug == TRUE) {
    message("Warning: rug is available only for continous variables!")
  }

  return(my_plot)
}
