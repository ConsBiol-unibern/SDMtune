#' Plot Response Curve
#'
#' Plot the Response Curve of the given environmental variable.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param var character. Name of the variable to be plotted.
#' @param type character. The output type used for "Maxent" and "Maxnet"
#' methods, possible values are "cloglog" and "logistic".
#' @param only_presence logical. If `TRUE` it uses only the presence locations
#' when applying the function for the marginal response.
#' @param marginal logical. If `TRUE` it plots the marginal response curve.
#' @param fun function used to compute the level of the other variables for
#' marginal curves.
#' @param rug logical. If `TRUE` it adds the rug plot for the presence and
#' absence/background locations, available only for continuous variables.
#' @param color The colour of the curve, default is "red".
#'
#' @details Note that fun is not a character argument, you must use `mean` and
#' not `"mean"`.
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
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
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "lq")
#'
#' # Plot cloglog response curve for a continuous environmental variable (bio1)
#' plotResponse(model,
#'              var = "bio1",
#'              type = "cloglog")
#'
#' # Plot marginal cloglog response curve for a continuous environmental
#' # variable (bio1)
#' plotResponse(model,
#'              var = "bio1",
#'              type = "cloglog",
#'              marginal = TRUE)
#'
#' # Plot logistic response curve for a continuous environmental variable
#' # (bio12) adding the rugs and giving a custom color
#' plotResponse(model,
#'              var = "bio12",
#'              type = "logistic",
#'              rug = TRUE,
#'              color = "blue")
#'
#' # Plot response curve for a categorical environmental variable (biome) giving
#' # a custom color
#' plotResponse(model,
#'              var = "biome",
#'              type = "logistic",
#'              color = "green")
#'
#' # Train a model with cross validation
#' folds <- randomFolds(data,
#'                      k = 4,
#'                      only_presence = TRUE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "lq",
#'                folds = folds)
#'
#' # Plot cloglog response curve for a continuous environmental variable (bio17)
#' plotResponse(model,
#'              var = "bio1",
#'              type = "cloglog")
#'
#' # Plot logistic response curve for a categorical environmental variable
#' # (biome) giving a custom color
#' plotResponse(model,
#'              var = "biome",
#'              type = "logistic",
#'              color = "green")
#' }
plotResponse <- function(model,
                         var,
                         type = NULL,
                         only_presence = FALSE,
                         marginal = FALSE,
                         fun = mean,
                         rug = FALSE,
                         color = "red") {

  if (!var %in% names(model@data@data))
    cli::cli_abort("Variable {.field {var}} is not used to train the model.")

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
    categ <- as.numeric(levels(df[, var]))
    n_rows <- length(categ)
  } else {
    n_rows <- 100
  }

  p_rug <- data.frame(x = p[, var])
  a_rug <- data.frame(x = a[, var])

  if (inherits(model, "SDMmodel")) {
    plot_data <- .get_plot_data(model, var, df, cont_vars, cat_vars, n_rows,
                                fun, marginal, type, categ)

    if (var %in% cont_vars) {
      my_plot <- ggplot(plot_data, aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_line(colour = color)

    } else {
      my_plot <- ggplot(plot_data, aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_bar(stat = "identity", fill = color)
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
      my_plot <- ggplot(plot_data,
                        aes(x = .data$x, y = .data$y, ymin = .data$y_min,
                            ymax = .data$y_max)) +
        ggplot2::geom_line(colour = color) +
        ggplot2::geom_ribbon(fill = color, alpha = 0.2)

    } else {
      my_plot <- ggplot(plot_data, aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_bar(stat = "identity", fill = color) +
        ggplot2::geom_errorbar(aes(ymin = .data$y_min, ymax = .data$y_max),
                               width = 0.2, linewidth = 0.3)
    }
  }

  my_plot <- my_plot +
    ggplot2::labs(x = var, y = ifelse(!is.null(type), paste(type, "output"),
                                      "Probability of presence")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(colour = "#666666"))

  if (rug == TRUE & var %in% cont_vars) {
    my_plot <- my_plot +
      ggplot2::geom_rug(data = p_rug, inherit.aes = FALSE, aes(.data$x),
                        sides = "t", color = "#4C4C4C") +
      ggplot2::geom_rug(data = a_rug, inherit.aes = FALSE, aes(.data$x),
                        sides = "b", color = "#4C4C4C")
  }

  my_plot
}


.get_plot_data <- function(model,
                           var,
                           df,
                           cont_vars,
                           cat_vars,
                           n_rows,
                           fun,
                           marginal,
                           type,
                           categ) {

  data <- data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
  colnames(data) <- colnames(df)
  data[cont_vars] <- apply(df[cont_vars], 2, fun)
  data[cat_vars] <- as.factor(apply(df[cat_vars], 2, terra::modal))
  data <- do.call("rbind", replicate(n_rows, data, simplify = FALSE))

  if (var %in% cont_vars) {
    var_min <- min(model@data@data[var])
    var_max <- max(model@data@data[var])
    data[var] <- seq(var_min, var_max, length.out = n_rows)
  } else {
    data[var] <- factor(categ)
  }

  if (!marginal) {
    new_data <- model@data
    new_data@data <- new_data@data[, var, drop = FALSE]
    settings <- list(data = new_data)
    model <- .create_model_from_settings(model, settings)
  }

  pred <- predict(model, data, type = type)

  data.frame(x = data[, var],
             y = pred)
}
