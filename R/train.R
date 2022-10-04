#' Train
#'
#' Train a model using one of the following methods: Artificial Neural Networks,
#' Boosted Regression Trees, Maxent, Maxnet or Random Forest.
#'
#' @param method character or character vector. Method used to train the model,
#' possible values are "ANN", "BRT", "Maxent", "Maxnet" or "RF", see details.
#' @param data \linkS4class{SWD} object with presence and absence/background
#' locations.
#' @param folds list. Output of the function \link{randomFolds} or folds object
#' created with other packages, see details.
#' @param progress logical. If `TRUE` shows a progress bar during cross
#' validation.
#' @param ... Arguments passed to the relative method, see details.
#'
#' @details
#' * For the ANN method possible arguments are (for more details see
#' \link[nnet]{nnet}):
#'     + size: integer. Number of the units in the hidden layer.
#'     + decay numeric. Weight decay, default is 0.
#'     + rang numeric. Initial random weights, default is 0.7.
#'     + maxit integer. Maximum number of iterations, default is 100.
#' * For the BRT method possible arguments are (for more details see
#' \link[gbm]{gbm}):
#'     + distribution: character. Name of the distribution to use, default is
#'     "bernoulli".
#'     + n.trees: integer. Maximum number of tree to grow, default is 100.
#'     + interaction.depth: integer. Maximum depth of each tree, default is 1.
#'     + shrinkage: numeric. The shrinkage parameter, default is 0.1.
#'     + bag.fraction: numeric. Random fraction of data used in the tree
#'     expansion, default is 0.5.
#' * For the RF method the model is trained as classification. Possible
#' arguments are (for more details see
#' \link[randomForest]{randomForest}):
#'     + mtry: integer. Number of variable randomly sampled at each split,
#'     default is `floor(sqrt(number of variables))`.
#'     + ntree: integer. Number of tree to grow, default is 500.
#'     + nodesize: integer. Minimum size of terminal nodes, default is 1.
#' * Maxent models are trained using the arguments
#' `"removeduplicates=false"` and `"addsamplestobackground=false"`.
#' Use the function \link{thinData} to remove duplicates and the function
#' \link{addSamplesToBg} to add presence locations to background locations. For
#' the Maxent method, possible arguments are:
#'     + reg: numeric. The value of the regularization multiplier, default is 1.
#'     + fc: character. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph".
#'     + iter: numeric. Number of iterations used by the MaxEnt algorithm,
#'       default is 500.
#' * Maxnet models are trained using the argument
#' `"addsamplestobackground = FALSE"`, use the function \link{addSamplesToBg}
#' to add presence locations to background locations. For the Maxnet method,
#' possible arguments are (for more details see \link[maxnet]{maxnet}):
#'     + reg: numeric. The value of the regularization intensity, default is 1.
#'     + fc: character. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph".
#'
#' The folds argument accepts also objects created with other packages:
#' \pkg{ENMeval} or \pkg{blockCV}. In this case the function converts
#' internally the folds into a format valid for \pkg{SDMtune}.
#'
#' When multiple methods are given as `method` argument, the function returns a
#' named list of model object, with the name corresponding to the used method,
#' see examples.
#'
#' @return An \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} or a list of
#' model objects.
#' @export
#'
#' @seealso \link{randomFolds}.
#'
#' @author Sergio Vignali
#'
#' @references
#' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S.
#' Fourth Edition. Springer, New York. ISBN 0-387-95457-0.
#'
#' Brandon Greenwell, Bradley Boehmke, Jay Cunningham and GBM Developers (2019).
#' gbm: Generalized Boosted Regression Models.
#' \url{https://CRAN.R-project.org/package=gbm}.
#'
#' A. Liaw and M. Wiener (2002). Classification and Regression by randomForest.
#' R News 2(3), 18--22.
#'
#' Hijmans, Robert J., Steven Phillips, John Leathwick, and Jane Elith. 2017.
#' dismo: Species Distribution Modeling.
#' \url{https://cran.r-project.org/package=dismo}.
#'
#' Steven Phillips (2017). maxnet: Fitting 'Maxent' Species Distribution Models
#' with 'glmnet'. \url{https://CRAN.R-project.org/package=maxnet}.
#'
#' Muscarella, R., Galante, P.J., Soley-Guardia, M., Boria, R.A., Kass, J.,
#' Uriarte, M. and R.P. Anderson (2014). ENMeval: An R package for conducting
#' spatially independent evaluations and estimating optimal model complexity
#' for ecological niche models. Methods in Ecology and Evolution.
#'
#' Roozbeh Valavi, Jane Elith, José Lahoz-Monfort and Gurutzeta Guillera-Arroita
#' (2018). blockCV: Spatial and environmental blocking for k-fold
#' cross-validation. \url{https://github.com/rvalavi/blockCV}.
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
#' ## Train a Maxent model
#' model <- train(method = "Maxent",
#'                data = data,
#'                fc = "l",
#'                reg = 1.5,
#'                iter = 700)
#'
#' # Add samples to background. This should be done preparing the data before
#' # training the model without using
#' data <- addSamplesToBg(data)
#' model <- train("Maxent",
#'                data = data)
#'
#' ## Train a Maxnet model
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "lq",
#'                reg = 1.5)
#'
#' ## Cross Validation
#' # Create 4 random folds splitting only the presence data
#' folds <- randomFolds(data,
#'                      k = 4,
#'                      only_presence = TRUE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                reg = 0.8,
#'                folds = folds)
#'
#' \dontrun{
#' # Run only if you have the package ENMeval installed
#' ## Block partition using the ENMeval package
#' require(ENMeval)
#' block_folds <- get.block(occ = data@coords[data@pa == 1, ],
#'                          bg.coords = data@coords[data@pa == 0, ])
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                reg = 0.8,
#'                folds = block_folds)
#'
#' ## Checkerboard1 partition using the ENMeval package
#' cb_folds <- get.checkerboard1(occ = data@coords[data@pa == 1, ],
#'                               env = predictors,
#'                               bg.coords = data@coords[data@pa == 0, ],
#'                               aggregation.factor = 4)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                reg = 0.8,
#'                folds = cb_folds)
#'
#' ## Environmental block using the blockCV package
#' # Run only if you have the package blockCV
#' require(blockCV)
#' # Create spatial points data frame
#' library(raster)
#' sp_df <- sp::SpatialPointsDataFrame(
#'   coords = data@coords,
#'   data = as.data.frame(data@pa),
#'   proj4string = sp::CRS(
#'   terra::crs(predictors))
#' )
#'
#' e_folds <- envBlock(rasterLayer = predictors,
#'                     speciesData = sp_df,
#'                     species = "data@pa",
#'                     k = 4,
#'                     standardization = "standard",
#'                     rasterBlock = FALSE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                reg = 0.8,
#'                folds = e_folds)
#' }
#'
#' ## Train presence absence models
#' # Prepare presence and absence locations
#' p_coords <- virtualSp$presence
#' a_coords <- virtualSp$absence
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = a_coords,
#'                    env = predictors[[1:5]])
#'
#' ## Train an Artificial Neural Network model
#' model <- train("ANN",
#'                data = data,
#'                size = 10)
#'
#' ## Train a Random Forest model
#' model <- train("RF",
#'                data = data,
#'                ntree = 300)
#'
#' ## Train a Boosted Regression Tree model
#' model <- train("BRT",
#'                data = data,
#'                n.trees = 300,
#'                shrinkage = 0.001)
#'
#' ## Multiple methods trained together with default arguments
#' output <- train(method = c("ANN", "BRT", "RF"),
#'                 data = data,
#'                 size = 10)
#' output$ANN
#' output$BRT
#' output$RF
#'
#' ## Multiple methods trained together passing extra arguments
#' output <- train(method = c("ANN", "BRT", "RF"),
#'                 data = data,
#'                 size = 10,
#'                 ntree = 300,
#'                 n.trees = 300,
#'                 shrinkage = 0.001)
#' output
#' }
train <- function(method,
                  data,
                  folds = NULL,
                  progress = TRUE,
                  ...) {

  l <- length(method)
  output <- vector("list", length = l)

  for (i in 1:l) {
    m <- match.arg(method[i], c("Maxent", "Maxnet", "ANN", "RF", "BRT"))
    func <- paste0("train", m)
    ea <- list(...)  # Extra arguments

    if (is.null(folds)) {
      argus <- c(data = data, ea[names(ea) %in% .args_name(func)])
      output[[i]] <- do.call(func, args = argus)
    } else {
      folds <- .convert_folds(folds, data)
      k <- ncol(folds[[1]])

      if (progress) {
        cli::cli_progress_bar(
          name = "Cross Validation",
          type = "iterator",
          format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | \\
              ETA: {cli::pb_eta} - {cli::pb_elapsed_clock}",
          total = k,
          clear = FALSE
        )
      }

      models <- vector("list", length = k)

      for (j in 1:k) {
        train <- .subset_swd(data, folds$train[, j])
        argus <- c(data = train, ea[names(ea) %in% .args_name(func)])
        models[[j]] <- do.call(func, args = argus)

        if (progress)
          cli::cli_progress_update()
      }

      output[[i]] <- SDMmodelCV(models = models, data = data, folds = folds)
    }
  }

  if (l == 1) {
    return(output[[1]])
  } else {
    names(output) <- method
    return(output)
  }
}
