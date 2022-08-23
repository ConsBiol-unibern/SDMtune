#' Predict for Cross Validation
#'
#' Predict the output for a new dataset given a trained \linkS4class{SDMmodelCV}
#' model. The output is given as the provided function applied to the prediction
#' of the k models.
#'
#' @param object \linkS4class{SDMmodelCV} object.
#' @param data data.frame, \linkS4class{SWD} or raster \link[raster]{stack} with
#' the data for the prediction.
#' @param fun character. function used to combine the output of the k models,
#' default is `"mean"`. Note that fun is a character argument, you must use
#' `"mean"` and not `mean`. You can also pass a vector of character containing
#' multiple function names, see details.
#' @param type character. Output type, see details, used only for **Maxent** and
#' **Maxnet** methods, default is `NULL`.
#' @param clamp logical for clumping during prediction, used only for **Maxent**
#' and **Maxnet** methods, default is `TRUE`.
#' @param filename character. Output file name for the prediction map, used only
#' when `data` is a \link[raster]{stack} object. If provided the output is saved
#' in a file, see details.
#' @param format character. The output format, see \link[raster]{writeRaster}
#' for all the options, default is "GTiff".
#' @param extent \link[raster]{extent} object, if provided it restricts the
#' prediction to the given extent, default is `NULL`.
#' @param ... Additional arguments to pass to the \link[raster]{writeRaster}
#' function.
#'
#' @details
#' * filename, format, extent, and ... arguments are used only when the
#' prediction is done for a \link[raster]{stack} object.
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
#' @return A vector with the prediction or a \link[raster]{raster} object if
#' data is a raster \link[raster]{stack} or a list in the case of multiple
#' functions.
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
#' # Create 4 random folds splitting only the presence data
#' folds <- randomFolds(data, k = 4, only_presence = TRUE)
#' model <- train(method = "Maxnet", data = data, fc = "l", folds = folds)
#'
#' # Make cloglog prediction for the all study area and get the result as
#' # average of the k models
#' predict(model, data = predictors, fun = "mean", type = "cloglog")
#'
#' # Make cloglog prediction for the all study area, get the average, standard
#' # deviation, and maximum values of the k models, and save the output in three
#' # files
#' \dontrun{
#' # The following commands save the output in the working directory
#' maps <- predict(model, data = predictors, fun = c("mean", "sd", "max"),
#'                 type = "cloglog", filename = "prediction")
#' # In this case three files are created: prediction_mean.tif,
#' # prediction_sd.tif and prediction_max.tif
#'
#' plotPred(maps$mean)
#' plotPred(maps$sd)
#' plotPred(maps$max)
#'
#' # Make logistic prediction for the all study area, given as standard
#' # deviation of the k models, and save it in a file
#' predict(model, data = predictors, fun = "sd", type = "logistic",
#'         filename = "my_map")
#' }
#' }
setMethod(
  "predict", signature = "SDMmodelCV",
  definition = function(object, data, fun = "mean", type = NULL,
                        clamp = TRUE, filename = "", format = "GTiff",
                        extent = NULL, ...) {

    k <- length(object@models)
    l <- length(fun)

    if (filename == "") {
      filename <- rep("", l)
    } else {
      filename <- paste(filename, fun, sep = "_")
    }

    pb <- progress::progress_bar$new(
      format = "Predict [:bar] :percent in :elapsedfull",
      total = k + l, clear = FALSE, width = 60, show_after = 0)
    pb$tick(0)

    # Create empty output list
    output <- vector("list", length = l)

    if (inherits(data, "Raster")) {
      preds <- vector("list", length = k)

      for (i in 1:k) {
        preds[[i]] <- predict(object@models[[i]], data = data, type = type,
                              clamp = clamp, extent = extent)
        pb$tick(1)
      }
      preds <- raster::stack(preds)

      for (i in 1:l) {
        output[[i]] <- raster::calc(preds, fun = get(fun[i]),
                                    filename = filename[i], format = format,
                                    ...)
        pb$tick(1)
      }
    } else {
      if (inherits(data, "SWD"))
        data <- data@data
      preds <- matrix(nrow = nrow(data), ncol = k)
      for (i in 1:k) {
        preds[, i] <- predict(object@models[[i]], data = data, type = type,
                              clamp = clamp, ...)
        pb$tick(1)
      }
      for (i in 1:l) {
        output[[i]] <- apply(preds, 1, get(fun[i]), na.rm = TRUE)
        pb$tick(1)
      }
    }

    if (l == 1) {
      return(output[[1]])
    } else {
      names(output) <- fun
      return(output)
    }
  })
