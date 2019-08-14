#' Plot Response Curve
#'
#' Plot the Response Curve of the given environmental variable.
#'
#' @param model \code{\linkS4class{SDMmodel}} or \code{\linkS4class{SDMmodelCV}}
#' object.
#' @param var character. Name of the variable to be plotted.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic", default is
#' \code{NULL}.
#' @param only_presence logical, f \code{TRUE} it uses only the range of the
#' presence location for the marginal response, default is \code{FALSE}.
#' @param marginal logical, if \code{TRUE} it plots the marginal response curve,
#' default is \code{FALSE}.
#' @param fun function used to compute the level of the other variables for
#' marginal curves, default is \code{mean}.
#' @param rug logical, if \code{TRUE} it adds the rug plot for the presence and
#' absence/background locations, available only for continuous variables,
#' default is \code{FALSE}.
#' @param color The color of the curve, default is "red".
#'
#' @details Note that fun is not a character argument, you must use mean and
#' not "mean".
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_line geom_bar scale_x_continuous
#' geom_ribbon geom_errorbar geom_rug labs theme
#' @importFrom raster modal
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
#' # Train a model
#' model <- train(method = "Maxnet", data = data, fc = "l")
#'
#' # Plot cloglog response curve for a continuous environmental variable (bio1)
#' plotResponse(model, var = "bio1", type = "cloglog")
#'
#' # Plot marginal cloglog response curve for a continuous environmental
#' # variable (bio1)
#' plotResponse(model, var = "bio1", type = "cloglog", marginal = TRUE)
#'
#' # Plot logistic response curve for a continuous environmental variable
#' # (bio12) adding the rugs and giving a custom color
#' plotResponse(model, var = "bio12", type = "logistic", rug = TRUE,
#'              color = "blue")
#'
#' # Plot response curve for a categorical environmental variable (biome) giving
#' # a custom color
#' plotResponse(model, var = "biome", type = "logistic", color = "green")
#'
#' # Train a model with cross validation
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "lq", rep = 4)
#'
#' # Plot cloglog response curve for a continuous environmental variable (bio17)
#' plotResponse(model, var = "bio1", type = "cloglog")
#'
#' # Plot logistic response curve for a categorical environmental variable
#' # (biome) giving a custom color
#' plotResponse(model, var = "biome", type = "logistic", color = "green")
#' }
plotResponse <- function(model, var, type = NULL, only_presence = FALSE,
                         marginal = FALSE, fun = mean, rug = FALSE,
                         color = "red") {

  if (!var %in% names(model@data@data))
    stop(paste(var, "is not used to train the model!"))

  p <- .get_presence(model@data)
  a <- .get_absence(model@data)

  if (only_presence) {
    df <- p
  } else {
    df <- model@data@data
  }

  cont_vars <- names(Filter(is.numeric, p))
  cat_vars <- names(Filter(is.factor, p))

  if (var %in% cat_vars) {
    categ <- unique(as.numeric(levels(df[, var]))[df[, var]])
    n_rows <- length(categ)
  } else {
    n_rows <- 100
  }

  p_rug <- data.frame(x = p[, var])
  a_rug <- data.frame(x = a[, var])

  if (class(model) == "SDMmodel") {
    plot_data <- .get_plot_data(model, var, df, cont_vars, cat_vars, n_rows,
                                fun, marginal, type, categ)

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
    plot_data <- .get_plot_data(model@models[[1]], var, df, cont_vars, cat_vars,
                                n_rows, fun, marginal, type, categ)
    colnames(plot_data) <- c("x", "y_1")
    for (i in 2:nf)
      plot_data[paste0("y_", i)] <- .get_plot_data(model@models[[i]], var, df,
                                                   cont_vars, cat_vars, n_rows,
                                                   fun, marginal, type, categ)$y
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
    labs(x = var, y = ifelse(!is.null(type), paste(type, "output"),
                             "Probability of presence")) +
    theme_minimal() +
    theme(text = element_text(colour = "#666666", family = "sans-serif"))

  if (rug == TRUE & var %in% cont_vars) {
    my_plot <- my_plot +
      geom_rug(data = p_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "t", color = "#4C4C4C") +
      geom_rug(data = a_rug, inherit.aes = FALSE, aes_string("x"),
               sides = "b", color = "#4C4C4C")
  }

  return(my_plot)
}


.get_plot_data <- function(model, var, df, cont_vars, cat_vars, n_rows, fun,
                           marginal, type, categ) {

  data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
  colnames(data) <- colnames(df)
  data[cont_vars] <- apply(df[cont_vars], 2, fun)
  data[cat_vars] <- as.factor(apply(df[cat_vars], 2, raster::modal))
  data <- do.call("rbind", replicate(n_rows, data, simplify = FALSE))

  if (var %in% cont_vars) {
    var_min <- min(df[var])
    var_max <- max(df[var])
    data[var] <- seq(var_min, var_max, length.out = n_rows)
  } else {
    data[var] <- categ
  }

  if (!marginal) {
    new_data <- model@data
    new_data@data <- new_data@data[, var, drop = FALSE]
    settings <- list(data = new_data)
    model <- .create_model_from_settings(model, settings)
  }

  pred <- predict(model, data, type = type)
  plot_data <- data.frame(x = data[, var], y = pred)

  return(plot_data)
}
