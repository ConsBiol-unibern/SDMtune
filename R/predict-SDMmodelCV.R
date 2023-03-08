#' Predict for Cross Validation
#'
#' Predict the output for a new dataset given a trained \linkS4class{SDMmodelCV}
#' model. The output is given as the provided function applied to the prediction
#' of the k models.
#'
#' @param object \linkS4class{SDMmodelCV} object.
#' @param data data.frame, \linkS4class{SWD} or raster \link[terra]{rast} with
#' the data for the prediction.
#' @param fun character. Function used to combine the output of the k models.
#' Note that fun is a character argument, you must use `"mean"` and not `mean`.
#' You can also pass a vector of character containing multiple function names,
#' see details.
#' @param type character. Output type, see details, used only for **Maxent** and
#' **Maxnet** methods.
#' @param clamp logical for clumping during prediction, used only for **Maxent**
#' and **Maxnet** methods.
#' @param filename character. If provided the raster map is saved in a file. It
#' must include the extension.
#' @param overwrite logical. If `TRUE` an existing file is overwritten.
#' @param wopt list. Writing options passed to \link[terra]{writeRaster}.
#' @param extent \link[terra]{ext} object, if provided it restricts the
#' prediction to the given extent.
#' @param progress logical. If `TRUE` shows a progress bar during prediction.
#' @param ... Additional arguments to pass to the \link[terra]{predict}
#' function.
#'
#' @details
#' * filename, and extent are arguments used only when the prediction is run for
#' a \link[terra]{rast} object.
#' * When a character vector is passed to the `fun` argument, than all the
#' given functions are applied and a named list is returned, see examples.
#' * When `filename` is provided and the `fun` argument contains more than one
#' function name, the saved files are named as `filename_fun`, see example.
#' * For models trained with the **Maxent** method the argument `type` can be:
#' "raw", "logistic" and "cloglog". The function performs the prediction in
#' **R** without calling the **MaxEnt** Java software. This results in a faster
#' computation for large datasets and might result in a slightly different
#' output compared to the Java software.
#' * For models trained with the **Maxnet** method the argument `type` can be:
#' "link", "exponential", "logistic" and "cloglog", see \link[maxnet]{maxnet}
#' for more details.
#' * For models trained with the **ANN** method the function uses the "raw"
#' output type.
#' * For models trained with the **RF** method the output is the probability of
#' class 1.
#' * For models trained with the **BRT** method the function uses the number of
#' trees defined to train the model and the "response" output type.
#'
#' @include SDMmodelCV-class.R
#'
#' @return A vector with the prediction or a \link[terra]{rast} object if data
#' is a \link[terra]{rast} or a list in the case of multiple functions.
#' @exportMethod predict
#'
#' @author Sergio Vignali
#'
#' @references Wilson P.D., (2009). Guidelines for computing MaxEnt model output
#' values from a lambdas file.
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
#' # Create 4 random folds splitting only the presence data
#' folds <- randomFolds(data,
#'                      k = 4,
#'                      only_presence = TRUE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                folds = folds)
#'
#' # Make cloglog prediction for the whole study area and get the result as
#' # average of the k models
#' predict(model,
#'         data = predictors,
#'         fun = "mean",
#'         type = "cloglog")
#'
#' # Make cloglog prediction for the whole study area, get the average, standard
#' # deviation, and maximum values of the k models, and save the output in three
#' # files
#' \dontrun{
#' # The following commands save the output in the working directory. Note that
#' # the filename must include the extension
#' maps <- predict(model,
#'                 data = predictors,
#'                 fun = c("mean", "sd", "max"),
#'                 type = "cloglog",
#'                 filename = "prediction.tif")
#'
#' # In this case three files are created: prediction_mean.tif,
#' # prediction_sd.tif and prediction_max.tif
#' plotPred(maps$mean)
#' plotPred(maps$sd)
#' plotPred(maps$max)
#'
#' # Make logistic prediction for the whole study area, given as standard
#' # deviation of the k models, and save it in a file
#' predict(model,
#'         data = predictors,
#'         fun = "sd",
#'         type = "logistic",
#'         filename = "my_map.tif")
#' }
#' }
setMethod(
  f = "predict",
  signature = "SDMmodelCV",
  definition = function(object,
                        data,
                        fun = "mean",
                        type = NULL,
                        clamp = TRUE,
                        filename = "",
                        overwrite = FALSE,
                        wopt = list(),
                        extent = NULL,
                        progress = TRUE,
                        ...) {

    k <- length(object@models)
    l <- length(fun)

    if (filename == "") {
      filename <- rep("", l)
    } else {
      file_ext <- tools::file_ext(filename)

      if (file_ext == "")
        cli::cli_abort(c(
          "x" = "Filename must include the extension"
        ))

      file_name <- tools::file_path_sans_ext(filename)
      filename <- paste0(file_name, "_", fun, ".", file_ext)
    }

    if (progress)
      cli::cli_progress_bar(
        name = stringr::str_glue("Predict - {class(object@models[[1]]@model)}"),
        type = "iterator",
        format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | \\
                ETA: {cli::pb_eta} - {cli::pb_elapsed_clock}",
        total = k + l,
        clear = FALSE
      )

    # Create empty output list
    output <- vector("list", length = l)

    # TODO: Remove with version 2.0.0
    if (inherits(data, "Raster")) {
      .raster_error("rast")
    }

    if (inherits(data, "SpatRaster")) {
      preds <- vector("list", length = k)

      if (!is.null(extent)) {

        # TODO: Remove with version 2.0.0
        if (inherits(extent, "Extent")) {
          .raster_error("ext")
        }

        if (inherits(extent, "SpatExtent")) {
          data <- terra::crop(data, extent)
        } else {
          cli::cli_abort(c(
            "!" = "{.var extent} must be a {.cls SpatExtent} object",
            "x" = "You have supplied a {.cls {class(extent)}} instead."
          ))
        }
      }

      for (i in 1:k) {
        preds[[i]] <- predict(object@models[[i]],
                              data = data,
                              type = type,
                              clamp = clamp)

        if (progress)
          cli::cli_progress_update()
      }

      preds <- terra::rast(preds)

      for (i in 1:l) {
        output[[i]] <- terra::app(preds,
                                  fun = get(fun[i]),
                                  filename = filename[i],
                                  wopt = wopt,
                                  ...)

        if (progress)
          cli::cli_progress_update()
      }
    } else if (inherits(data, c("SWD", "data.frame"))) {

      if (inherits(data, "SWD"))
        data <- data@data

      preds <- matrix(nrow = nrow(data), ncol = k)

      for (i in 1:k) {
        preds[, i] <- predict(object@models[[i]],
                              data = data,
                              type = type,
                              clamp = clamp,
                              ...)

        if (progress)
          cli::cli_progress_update()
      }

      for (i in 1:l) {
        output[[i]] <- apply(preds, 1, get(fun[i]), na.rm = TRUE)

        if (progress)
          cli::cli_progress_update()
      }
    } else {
      cli::cli_abort(c(
        "!" = paste("{.var data} must be an object of class",
                    "{.cls data.frame}, {.cls SWD} or {.cls SpatRaster}"),
        "x" = "You have supplied a {.cls {class(data)}} instead."
      ))
    }

    if (l == 1) {
      return(output[[1]])
    } else {
      names(output) <- fun
      return(output)
    }
  }
)
