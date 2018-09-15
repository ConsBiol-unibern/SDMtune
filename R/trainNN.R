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
#' @importFrom keras %>% compile fit
#'
#' @examples \dontrun{
#' trainNN(presence, bg, conf = list(c(100, "tanh"), c(100, "tanh")))}
#'
#' @author Sergio Vignali
trainNN <- function(presence, bg, conf = NULL, model = NULL, reg = 0,
                    optimizer = "rmsprop", loss = "mse", epoch = 500,
                    batch_size = 32, verbose = 1, callbacks = list()) {

  if (is.null(conf) & is.null(model))
    stop("You must provide conf or model parameter!")
  requireNamespace("keras")

  result <- SDMmodel(presence = presence, background = bg)
  x <- rbind(presence@data, bg@data)
  cont_vars <- names(Filter(is.numeric, x))
  cat_vars <- names(Filter(is.factor, x))
  xlevs <- lapply(bg@data[cat_vars], function(i) {levels(i)})
  min_max <- data.frame(variable = colnames(x[cont_vars]),
                        min = apply(x[cont_vars], 2, min),
                        max = apply(x[cont_vars], 2, max))

  # Standardize continuous variables
  means <- apply(x[cont_vars], 2, mean)
  stds <- apply(x[cont_vars], 2, sd)

  x[cont_vars] <- scale(x[cont_vars], center = means, scale = stds)

  if (length(cat_vars) > 0) {
    for (cat in cat_vars) {
      one_hot <- one_hot(x[, cat], levels(x[, cat]))
      colnames(one_hot) <- paste0(cat, "_", 1:ncol(one_hot))
      x[cat] <- NULL
      x <- cbind(x, one_hot)
    }
  }

  x <- data.matrix(x)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))

  if (!is.null(model)) {
    model <- model
  } else {
    model <- parse_nn(conf, reg, ncol(x))
  }

  model %>% compile(optimizer = optimizer, loss = loss)

  history <- model %>% fit(x, p, epochs = epoch, batch_size = batch_size,
                           callbacks = callbacks, verbose = verbose)
  model_object <- NN(model = model, loss = loss, optimizer = optimizer,
               min_max = min_max, means = means, stds = stds, levels = xlevs)

  result@model <- model_object

  return(list(result, history))
}

#' @importFrom keras %>% keras_model_sequential layer_dense regularizer_l2
parse_nn <- function(conf, reg, input_units) {
  model <- keras_model_sequential()
  for (i in 1:length(conf)) {
    if (i == 1) {
      model <- model %>% layer_dense(units = conf[[i]][1],
                                     activation = conf[[i]][2],
                                     regularizer_l2(reg),
                                     input_shape = input_units)
    } else {
      model <- model %>% layer_dense(units = conf[[i]][1],
                                   activation = conf[[i]][2],
                                   regularizer_l2(reg))
    }
  }
  model <- model %>% layer_dense(units = 1, activation = "sigmoid",
                                 regularizer_l2(reg))
  return(model)
}
