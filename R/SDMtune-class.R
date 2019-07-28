#' SDMtune class
#'
#' Class used to save the results of one of the following functions:
#' \link{gridSearch}, \link{randomSearch} or \link{optimizeModel}.
#'
#' @slot results data.frame. Results with the evaluation of the models.
#' @slot models list. List of \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV}
#' objects.
#'
#' @export
#'
#' @author Sergio Vignali
SDMtune <- setClass("SDMtune",
                    slots = c(results = "data.frame",
                              models = "list")
                    )

setMethod("show",
  signature = "SDMtune",
  definition = function(object) {

    tunable_hypers <- get_tunable_args(object@models[[1]])

    cat("Object of class: ", class(object), "\n\n")

    cat("Models configuration:\n")
    cat("--------------------\n")

    for (i in 1:length(tunable_hypers)) {
      h <- paste(sort(unique(object@results[, tunable_hypers[i]])),
                 collapse = ", ")
      cat(tunable_hypers[i], ": ", h, "\n", sep = "")
    }
  }
)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot SDMtune object
#'
#' Plot an \linkS4class{SDMtune} object. Use the interactive argument to create
#' an interactive chart.
#'
#' @param x \linkS4class{SDMtune} object.
#' @param title character. The title of the plot, by default is an empty string.
#' @param interactive logical, if TRUE plot an interactive chart, default is
#' \code{FALSE}.
#'
#' @rdname plot-methods
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot aes_string geom_point labs scale_color_manual
#' theme_minimal theme element_text geom_line
#' @exportMethod plot
#'
#' @return If \code{interactive = FALSE} the function returns a
#' \code{\link[ggplot2]{ggplot}} object otherwise it returns an SDMtuneChart
#' object that contains the path of the temporary folder where the necessary
#' files to create the chart are saved. This object can be used to saved the
#' chart in a file using the \link{saveChart} function. In both cases the
#' objects are returned as invisible.
#'
#' @seealso \link{saveChart}
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
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' bg <- prepareSWD(species = "Vultur gryphus", coords = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "l")
#'
#' # Define the hyperparameters to test
#' h <- list(reg = 1:2, fc = c("lqp", "lqph"), a = c(1000, 2000))
#'
#' # Run the gridSearch function using as metric the AUC
#' output <- gridSearch(model, hypers = h, metric = "auc", test = test,
#'                      bg4test = bg)
#'
#' # Plot the output
#' plot(output, title = "My experiment")
#'
#' # Plot the interactive chart
#' p <- plot(output, title = "My experiment", interactive = TRUE)
#' p
#' # Print the temporary folder that stores the files used to create the chart
#' str(p)
#'}
setMethod("plot",
  signature(x = "SDMtune", y = "missing"),
  definition = function(x, title = "", interactive = FALSE) {

    if (interactive) {
      folder <- structure(tempfile("SDMtune"), class = "SDMtuneChart")
      .create_plot(x, title, interactive = TRUE, folder = folder)
      return(folder)
    } else {
      p <- .create_plot(x, title, interactive = FALSE)
      return(p)
    }
  })
