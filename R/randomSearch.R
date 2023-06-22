#' Random Search
#'
#' The function performs a random search in the hyperparameters space, creating
#' a population of random models each one with a random combination of the
#' provided hyperparameters values.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperparameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \linkS4class{SWD} object. Test dataset used to evaluate the
#' model, not used with aicc and \linkS4class{SDMmodelCV} objects.
#' @param pop numeric. Size of the population.
#' @param env \link[terra]{rast} containing the environmental variables, used
#' only with "aicc".
#' @param interactive logical. If `FALSE` the interactive chart is not created.
#' @param progress logical. If `TRUE` shows a progress bar.
#' @param seed numeric. The value used to set the seed to have consistent
#' results.
#'
#' @details To know which hyperparameters can be tuned you can use the output
#' of the function \link{getTunableArgs}. Hyperparameters not included in the
#' `hypers` argument take the value that they have in the passed model.
#'
#' An interactive chart showing in real-time the steps performed by the
#' algorithm is displayed in the Viewer pane.
#'
#' @return \linkS4class{SDMtune} object.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{# Acquire environmental variables
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
#' # Define the hyperparameters to test
#' h <- list(reg = seq(0.2, 3, 0.2),
#'           fc = c("lqp", "lqph", "lh"))
#'
#' # Run the function using as metric the AUC
#' output <- randomSearch(model,
#'                        hypers = h,
#'                        metric = "auc",
#'                        test = test,
#'                        pop = 10,
#'                        seed = 25)
#' output@results
#' output@models
#'
#' # Order results by highest test AUC
#' output@results[order(-output@results$test_AUC), ]}
randomSearch <- function(model,
                         hypers,
                         metric,
                         test = NULL,
                         pop = 20,
                         env = NULL,
                         interactive = TRUE,
                         progress = TRUE,
                         seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  optimizeModel(model = model,
                hypers = hypers,
                test = test,
                metric = metric,
                pop = pop,
                gen = 0,
                env = env,
                interactive = interactive,
                progress = progress,
                seed = seed)
}
