#' Plot Response Curve
#'
#' Plot the Response Curve of the given environmental variable.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param var character. Name of the variable to be plotted.
#' @param type character. Output type, see \link{predict,Maxent-method} for
#' Maxent models or \link{predict.maxnet} for Maxnet models.
#' @param marginal logical, if TRUE it plots the marginal response courve,
#' default is FALSE.
#' @param fun function used to compute the level of the other variables for
#' marginal curves, possible values are mean and median, default is mean.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param rug logical, if TRUE it adds the rug plot for the presence and
#' absence/background locations, available only for continuous variables,
#' default is FALSE.
#' @param color The color of the curve, default is "red".
#'
#' @details Note that fun is not a character argument, you must use mean and
#' not "mean".
#'
#' @return The plot model
#' @export
#' @include Maxent_class.R
#' @importFrom ggplot2 ggplot aes_string geom_line geom_bar scale_x_continuous
#' geom_ribbon geom_errorbar geom_rug labs theme
#' @importFrom raster modal
#'
#' @examples
#' \dontrun{
#' plotResponse(model, var = "bio12", marginal = T, fun = median, rug = T)}
#'
#' @author Sergio Vignali
plotResponse <- function(model, var, type, marginal = FALSE, fun = mean,
                         clamp = TRUE, rug = FALSE, color = "red") {

  if (!var %in% names(model@p@data))
    stop(paste(var, "is not used to train the model!"))

  if (class(model) == "SDMmodel") {
    a <- model@a
  } else {
    a <- model@models[[1]]@a
  }

  train <- model@p
  cont_vars <- names(Filter(is.numeric, a@data))
  cat_vars <- names(Filter(is.factor, a@data))

  if (var %in% cat_vars) {
    categ <- unique(as.numeric(levels(a@data[, var]))[a@data[, var]])
    n_rows <- length(categ)
  } else {
    n_rows <- 100
  }

  train_rug <- data.frame(x = train@data[, var])
  a_rug <- data.frame(x = a@data[, var])

  if (class(model) == "SDMmodel") {
    plot_data <- get_plot_data(model, train, a, var, cont_vars, cat_vars,
                               n_rows, train_rug, fun, marginal, clamp, type,
                               categ)

    if (var %in% cont_vars) {
      my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
        geom_line(colour = color)

    } else {
      my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
        geom_bar(stat = "identity", fill = color) +
        scale_x_continuous(breaks = seq(min(plot_data$x),
                                        max(plot_data$x), 1))
    }
  } else {
    nf <- length(model@models)
    plot_data <- get_plot_data(model@models[[1]], train, a, var, cont_vars,
                               cat_vars, n_rows, train_rug, fun, marginal,
                               clamp, type, categ)
    colnames(plot_data) <- c("x", "y_1")
    for (i in 2:nf)
      plot_data[paste0("y_", i)] <- get_plot_data(model@models[[i]], train, a,
                                                  var, cont_vars, cat_vars,
                                                  n_rows, train_rug, fun,
                                                  marginal, clamp, type)$y
    plot_data$y <- rowMeans(plot_data[, -1])
    plot_data$sd <- apply(plot_data[, 2:(nf + 1)], 1, sd, na.rm = TRUE)
    plot_data$y_min <- plot_data$y - plot_data$sd
    plot_data$y_max <- plot_data$y + plot_data$sd

    if (var %in% cont_vars) {
      my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y", ymin = "y_min",
                                              ymax = "y_max")) +
        geom_line(colour = color) +
        geom_ribbon(fill = color, alpha = 0.2)

    } else {
      my_plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
        geom_bar(stat = "identity", fill = color) +
        geom_errorbar(aes_string(ymin = "y_min", ymax = "y_max"),
                      width = 0.2, size = 0.3) +
        scale_x_continuous(breaks = seq(min(plot_data$x),
                                        max(plot_data$x), 1))
    }
  }

  my_plot <- my_plot +
    labs(x = var, y = paste(type, "output")) +
    theme_minimal() +
    theme(text = element_text(colour = "#666666", family = "sans-serif"))

  if (rug == TRUE & var %in% cont_vars) {
    my_plot <- my_plot +
      geom_rug(data = train_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "t", color = "#4C4C4C") +
      geom_rug(data = a_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "b", color = "#4C4C4C")
  }

  return(my_plot)
}


get_plot_data <- function(model, train, a, var, cont_vars, cat_vars, n_rows,
                          train_rug, fun, marginal, clamp, type, categ) {

  data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(train@data)))
  colnames(data) <- colnames(train@data)
  data[cont_vars] <- apply(train@data[cont_vars], 2, fun)
  data[cat_vars] <- as.factor(apply(train@data[cat_vars], 2, raster::modal))
  data <- do.call("rbind", replicate(n_rows, data, simplify = FALSE))

  if (clamp & var %in% cont_vars) {
    var_min <- min(a@data[var])
    var_max <- max(a@data[var])
    train_rug <- data.frame(x = clamp(train_rug$x, var_min, var_max))
  } else if (var %in% cont_vars) {
    var_min <- min(rbind(train@data[var], a@data[var]))
    var_max <- max(rbind(train@data[var], a@data[var]))
  }

  if (var %in% cont_vars) {
    data[var] <- seq(var_min, var_max, length.out = n_rows)
  } else {
    data[var] <- categ
  }

  if (!marginal) {
    train@data <- model@p@data[var]
    a@data <- model@a@data[var]
    settings <- list("p" = train, "a" = a)

    model <- .create_model_from_settings(model, settings)
  }

  pred <- predict(model, data, type = type, clamp = clamp)
  plot_data <- data.frame(x = data[, var], y = pred)

  return(plot_data)
}
