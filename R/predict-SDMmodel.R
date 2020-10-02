setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict
#'
#' Predict the output for a new dataset given a trained \linkS4class{SDMmodel}
#' model.
#'
#' @param object \linkS4class{SDMmodel} object.
#' @param data data.frame, \linkS4class{SWD} or \link[raster]{stack} with the
#' data for the prediction.
#' @param type character. Output type, see details, used only for **Maxent** and
#' **Maxnet** methods, default is `NULL`.
#' @param clamp logical for clumping during prediction, used only for **Maxent**
#' and **Maxnet** methods, default is `TRUE`.
#' @param filename character. Output file name for the prediction map, used only
#' when `data` is a \link[raster]{stack} object. If provided the output is saved
#' in a file.
#' @param format character. The output format, see \link[raster]{writeRaster}
#' for all the options, default is "GTiff".
#' @param extent \link[raster]{extent} object, if provided it restricts
#' the prediction to the given extent, default is `NULL`.
#' @param progress character to display a progress bar: "text", "window" or ""
#' (default) for no progress bar.
#' @param ... Additional arguments to pass to the \link[raster]{writeRaster}
#' function.
#'
#' @details
#' * filename, format, extent, progress, and ... are arguments used only when
#' the prediction is done for a \link[raster]{stack} object.
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
#' @return A vector with the prediction or a \link[raster]{raster} object if
#' data is a raster \link[raster]{stack}.
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Make cloglog prediction for the test dataset
#' predict(model, data = test, type = "cloglog")
#'
#' # Make logistic prediction for the all study area
#' predict(model, data = predictors, type = "logistic")
#'
#' \dontrun{
#' # Make logistic prediction for the all study area and save it in a file
#' # The function saves the file in your working directory
#' predict(model, data = predictors, type = "logistic", filename = "my_map")
#' }
setMethod("predict",
          signature = "SDMmodel",
          definition = function(object, data, type = NULL, clamp = TRUE,
                                filename = "", format = "GTiff", extent = NULL,
                                progress = "", ...) {

            if (class(object@model) != "Maxnet") {
              model <- object@model
            } else {
              model <- object@model@model
            }

            vars <- colnames(object@data@data)

            if (inherits(data, "Raster")) {
              data <- raster::subset(data, vars)
              pred <- raster::predict(data,
                                      model = model,
                                      type = type,
                                      clamp = clamp,
                                      fun = predict,
                                      progress = progress,
                                      filename = filename,
                                      format = format,
                                      ext = extent,
                                      ...)
            } else if (inherits(data, "SWD")) {
              data <- data@data[vars]
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            } else if (inherits(data, "data.frame")) {
              data <- data[vars]
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            }

            return(pred)
          })
