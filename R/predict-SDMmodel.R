setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict
#'
#' Predict the output for a new dataset given a trained \linkS4class{SDMmodel}
#' model.
#'
#' @param object \linkS4class{SDMmodel} object.
#' @param data data.frame, \linkS4class{SWD} or \link[terra]{rast} with the data
#' for the prediction.
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
#' @param ... Additional arguments to pass to the \link[terra]{predict}
#' function.
#'
#' @details
#' * filename, and extent are arguments used only when the prediction is run for
#' a \link[terra]{rast} object.
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
#' @include Maxent-class.R Maxnet-class.R ANN-class.R RF-class.R BRT-class.R
#' @import methods
#'
#' @return A vector with the prediction or a \link[terra]{rast} object if
#' data is a raster \link[terra]{rast}.
#' @exportMethod predict
#'
#' @author Sergio Vignali
#'
#' @references Wilson P.D., (2009). Guidelines for computing MaxEnt model output
#' values from a lambdas file.
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
#' # Make cloglog prediction for the test dataset
#' predict(model,
#'         data = test,
#'         type = "cloglog")
#'
#' # Make logistic prediction for the whole study area
#' predict(model,
#'         data = predictors,
#'         type = "logistic")
#'
#' \dontrun{
#' # Make logistic prediction for the whole study area and save it in a file.
#' # Note that the filename must include the extension. The function saves the
#' # file in your working directory
#' predict(model,
#'         data = predictors,
#'         type = "logistic",
#'         filename = "my_map.tif")}
setMethod(
  f = "predict",
  signature = "SDMmodel",
  definition = function(object,
                        data,
                        type = NULL,
                        clamp = TRUE,
                        filename = "",
                        overwrite = FALSE,
                        wopt = list(),
                        extent = NULL,
                        ...) {

    model <- object@model

    vars <- colnames(object@data@data)

    # TODO: Remove with version 2.0.0
    if (inherits(data, "Raster")) {
      .raster_error("rast")
      data <- terra::rast(data)
    }

    if (inherits(data, "SpatRaster")) {
      data <- terra::subset(data, vars)

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

      if (filename != "") {
        file_ext <- tools::file_ext(filename)

        if (file_ext == "")
          cli::cli_abort(c(
            "x" = "Filename must include the extension"
          ))
      }

      pred <- terra::predict(data,
                             model = model,
                             fun = predict,
                             # Arguments for SDMtune predict functions
                             type = type,
                             clamp = clamp,
                             # Additional arguments for terra predict function
                             filename = filename,
                             na.rm = TRUE,
                             overwrite = overwrite,
                             wopt = wopt,
                             ...)
    } else if (inherits(data, "SWD")) {
      data <- data@data[vars]
      pred <- predict(model, data, type = type, clamp = clamp)
      pred <- as.vector(pred)
    } else if (inherits(data, "data.frame")) {
      data <- data[vars]
      pred <- predict(model, data, type = type, clamp = clamp)
      pred <- as.vector(pred)
    } else {
      cli::cli_abort(c(
        "!" = paste("{.var data} must be an object of class",
                    "{.cls data.frame}, {.cls SWD} or {.cls SpatRaster}"),
        "x" = "You have supplied a {.cls {class(data)}} instead."
      ))
    }

    return(pred)
  }
)
