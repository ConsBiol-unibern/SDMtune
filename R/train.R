#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent", "Maxnet" or "NN".
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param reg numeric. The value of the regularization intensiy.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t", not used for "NN".
#' @param conf list containing vector with layers configuration, see details,
#' used only for "NN".
#' @param model a keras model, used only for "NN".
#' @param optimizer character. The optimizer algorithm, default is "rmsprop",
#' used only for "NN".
#' @param loss character. The Loss function to be optimized, default is "mse",
#' used only for "NN".
#' @param epoch integer. Number of epoch used to train the model,
#' default is 500, used only for "NN".
#' @param batch_size integer. Number of samples used in each mini batch,
#' default is 32, used only for "NN".
#' @param verbose integer. Verbosity mode (0 = silent, 1 = progress bar,
#' 2 = text), used only for "NN".
#' @param callbacks List of callbacks to be called during training, used only
#' for "NN".
#' @param iter numeric. Number of iterations used by the Maxent alghoritm,
#' default is 500, used only for "maxent" method.
#' @param extra_args vector. Extra arguments used to run MaxEnt, default is
#' c("noaddsamplestobackground", "removeduplicates=false"), used only for
#' "maxent" method.#'
#'
#' @details See \link{trainMaxent}, \link{trainMaxnet} or \link{trainNN} for
#' details related to the different methods. For **Maxent** models the function
#' uses by default
#' **extra_args = c("noaddsamplestobackground","removeduplicates=false")**. In
#' case this is not your expected beaviour you can remove both passing
#' **extra_args = ""** or you can add any other additional arguments extending
#' the previous vector.
#'
#' @return A SWDmodel object
#' @export
#'
#' @examples \dontrun{
#' model <- train("Maxnet", presence, bg, reg = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
train <- function(method = c("Maxent", "Maxnet", "NN"), presence, bg, reg,
                  fc = NULL, conf = NULL, model = NULL, optimizer = "rmsprop",
                  loss = "mse", epoch = 500, batch_size = 32, verbose = 1,
                  callbacks = list(), iter = 500,
                  extra_args = c("noaddsamplestobackground",
                                 "removeduplicates=false")) {
  method = match.arg(method)

  if (method == "Maxent") {
    model <- trainMaxent(presence = presence, bg = bg, reg = reg, fc = fc,
                         iter = iter, extra_args = extra_args)
  } else if (method == "Maxnet") {
    model <- trainMaxnet(presence = presence, bg = bg, reg = reg, fc = fc)
  } else {
    model <- trainNN(presence = presence, bg = bg, conf = conf, model = model,
                     reg = reg, optimizer = optimizer, loss = loss,
                     epoch = epoch, batch_size = batch_size, verbose = verbose,
                     callbacks = callbacks)
  }

  return(model)
}
