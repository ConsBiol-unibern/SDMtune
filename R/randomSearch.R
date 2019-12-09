#' Random Search
#'
#' The function performs a random search in the hyperparameters space, creating
#' a population of random models each one with a random combination of the
#' provided hyperparameters values.
#'
#' @param model \code{\linkS4class{SDMmodel}} or \code{\linkS4class{SDMmodelCV}}
#' object.
#' @param hypers named list containing the values of the hyperparameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \code{\linkS4class{SWD}} object. Test dataset used to evaluate
#' the model, not used with aicc and \code{\linkS4class{SDMmodelCV}} objects,
#' default is \code{NULL}.
#' @param pop numeric. Size of the population, default is 20.
#' @param env \code{\link[raster]{stack}} containing the environmental
#' variables, used only with "aicc", default is \code{NULL}.
#' @param parallel logical, if \code{TRUE} it uses parallel computation, default
#' is \code{FALSE}. Used only with \code{metric = "aicc"}, see details.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is \code{NULL}.
#'
#' @details * To know which hyperparameters can be tuned you can use the output
#' of the function \code{\link{get_tunable_args}}. Hyperparameters not included
#' in the \code{hypers} argument take the value that they have in the passed
#' model.
#' * Parallel computation is used only during the execution of the predict
#' function, and increases the speed only for large datasets. For small dataset
#' it may result in a longer execution, due to the time necessary to create the
#' cluster.
#'
#' @return \code{\linkS4class{SDMtune}} object.
#' @export
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Define the hyperparameters to test
#' h <- list(reg = seq(0.2, 3, 0.2), fc = c("lqp", "lqph", "lh"))
#'
#' # Run the function using as metric the AUC
#' output <- randomSearch(model, hypers = h, metric = "auc", test = test,
#'                        pop = 10, seed = 25)
#' output@results
#' output@models
#' # Order rusults by highest test AUC
#' output@results[order(-output@results$test_AUC), ]
#' }
randomSearch <- function(model, hypers, metric, test = NULL, pop = 20,
                         env = NULL, parallel = FALSE, seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  output <- optimizeModel(model = model, hypers = hypers, test = test,
                          metric = metric, pop = pop, gen = 0, env = env,
                          parallel = parallel, seed = seed)

  return(output)
}
