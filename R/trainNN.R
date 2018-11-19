#' Train Neural Network
#'
#' Train a Neural Network model
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param conf list containing vector with layers configuration, see details.
#' @param model a keras model.
#' @param reg numeric. The regularization to be applied to each layer,
#' default is 0.
#' @param optimizer character. The optimizer algorithm, default is "rmsprop".
#' @param loss character. The Loss function to be optimized, default is "mse".
#' @param epoch integer. Number of epoch used to train the model,
#' default is 500.
#' @param batch_size integer. Number of samples used in each mini batch,
#' default is 32.
#' @param verbose integer. Verbosity mode (0 = silent, 1 = progress bar,
#' 2 = text).
#' @param callbacks List of callbacks to be called during training.
#'
#' @details Write something here...
#'
#' @return NN object.
#' @export
#' @importFrom keras %>% compile fit model_to_yaml
#'
#' @examples \dontrun{
#' trainNN(presence, bg, conf = list(c(100, "tanh"), c(100, "tanh")))}
#'
#' @author Sergio Vignali
trainNN <- function(presence, bg, conf = NULL, model = NULL, reg = 0,
                    optimizer = "rmsprop", loss = "binary_crossentropy",
                    epoch = 500, batch_size = 32, verbose = 1,
                    callbacks = list()) {

  if (is.null(conf) & is.null(model))
    stop("You must provide conf or model parameter!")
  requireNamespace("keras")

  result <- SDMmodel(presence = presence, background = bg)
  x <- rbind(presence@data, bg@data)
  cont_vars <- names(Filter(is.numeric, x))
  cat_vars <- names(Filter(is.factor, x))
  levels <- lapply(bg@data[cat_vars], function(i) {levels(i)})
  min_max <- data.frame(variable = colnames(x[cont_vars]),
                        min = apply(x[cont_vars], 2, min),
                        max = apply(x[cont_vars], 2, max))

  # Standardize continuous variables
  means <- apply(x[cont_vars], 2, mean)
  stds <- apply(x[cont_vars], 2, sd)

  x <- format_data(x, means, stds, levels)
  x <- data.matrix(x)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))

  if (!is.null(model)) {
    model <- model
  } else {
    model <- parseNNConf(conf, reg, ncol(x))
  }

  model %>% compile(optimizer = optimizer, loss = loss)
  yaml <- keras::model_to_yaml(model)

  history <- model %>% fit(x, p, epochs = epoch, batch_size = batch_size,
                           callbacks = callbacks, verbose = verbose)
  model_object <- NN(model = model, history = history, loss = loss,
                     optimizer = optimizer, epoch = epoch,
                     batch_size = batch_size, callbacks = callbacks,
                     min_max = min_max, means = means, stds = stds,
                     levels = levels, yaml = yaml)

  result@model <- model_object

  return(result)
}
