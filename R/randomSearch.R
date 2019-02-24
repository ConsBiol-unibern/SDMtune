#' Random Search
#'
#' The function performs a random search in the hyperparameters space, creating
#' a population of random models each one with a random combination of the
#' provided hyperparameters values.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperpatameters that
#' should be tuned, see details.
#' @param bg4test \link{SWD} object or NULL. Background locations used to get
#' subsamples if **a** hyperparameter is tuned, default is NULL.
#' @param test \link{SWD} object. Test dataset used to evaluate the model, not
#' used with aicc and \link{SDMmodelCV} objects, default is NULL.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param pop numeric. Size of the population, default is 20.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is NULL.
#'
#' @details To know which hyperparameters can be tune you can use the output of
#' the function \link{get_tunable_args}.
#'
#' @return \link{SDMtune} object.
#' @export
#'
#' @examples \dontrun{output <- optimiseModel(my_model, hypers = list( "reg" =
#' c(0.5, 1, 1.5), "fc" = c("lq", "lqp", "lqph"), "a" = c(5000, 10000, 15000)),
#' bg4test = bg, test = my_val, pop = 30, seed = 25)}
#'
#' @author Sergio Vignali
randomSearch <- function(model, hypers, bg4test = NULL, test = NULL,
                         metric = c("auc", "tss", "aicc"), pop = 20, env = NULL,
                         parallel = FALSE, seed = NULL) {

  metric = match.arg(metric)

  output <- optimiseModel(model = model, hypers = hypers, bg4test = bg4test,
                          test = test, metric = metric, gen = 0, env = env,
                          parallel = parallel, seed = seed)

  return(output)
}
