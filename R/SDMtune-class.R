#' SDMtune class
#'
#' Class used to save the results of one of the following functions:
#' \link{gridSearch}, \link{randomSearch} or \link{optimizeModel}.
#'
#' @slot results data.frame. Results with the evaluation of the models.
#' @slot models list. List of \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV}
#' objects.
#' @rdname SDMtune-class
#' @export
#'
#' @author Sergio Vignali
SDMtune <- setClass("SDMtune",
                    slots = c(results = "data.frame",
                              models = "list")
                    )

#' @param object SDMtune object
#' @rdname SDMtune-class
setMethod("show",
  signature = "SDMtune",
  definition = function(object) {

    tunable_hypers <- getTunableArgs(object@models[[1]])

    cli::cli_h2("Object of class: {.cls {class(object)}}")

    cli::cli_par()
    cli::cli_text("Method: {.emph { .get_method(object@models[[1]])}}")
    cli::cli_end()

    cli::cli_h3("Tested hyperparameters")

    for (i in seq_along(tunable_hypers)) {
      h <- sort(unique(object@results[, tunable_hypers[i]]))
      cli::cli_li("{.field {tunable_hypers[i]}}: {.val {h}}")
    }
  }
)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot SDMtune object
#'
#' Plot an SDMtune object. Use the interactive argument to create
#' an interactive chart.
#'
#' @param x SDMtune object.
#' @param title character. The title of the plot.
#' @param interactive logical, if TRUE plot an interactive chart.
#'
#' @rdname SDMtune-class
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes
#' @exportMethod plot
#'
#' @return If \code{interactive = FALSE} the function returns a
#' \code{\link[ggplot2]{ggplot}} object otherwise it returns an SDMtuneChart
#' object that contains the path of the temporary folder where the necessary
#' files to create the chart are saved. In both cases the objects are returned
#' as invisible.
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- terra::rast(files)
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
#' # Define the hyperparameters to test
#' h <- list(reg = 1:5, fc = c("lqp", "lqph"))
#'
#' # Run the gridSearch function using as metric the AUC
#' output <- gridSearch(model, hypers = h, metric = "auc", test = test)
#' output
#'
#' # Plot the output
#' plot(output, title = "My experiment")
#'
#' # Plot the interactive chart
#' p <- plot(output, title = "My experiment", interactive = TRUE)
#' # Print the temporary folder that stores the files used to create the chart
#' str(p)
#'}
setMethod("plot",
  signature(x = "SDMtune", y = "missing"),
  definition = function(x, title = "", interactive = FALSE) {

    if (interactive) {
      folder <- structure(tempfile("SDMtune"), class = "SDMtuneChart")
      .create_plot(x, title, interactive = TRUE, folder = folder)
      .show_chart(folder)
      return(invisible(folder))
    } else {
      p <- .create_plot(x, title, interactive = FALSE)
      return(p)
    }
  })
