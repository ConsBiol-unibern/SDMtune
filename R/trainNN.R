#' Train Neural Network
#'
#' @param presence
#' @param bg
#' @param conf
#' @param model
#' @param reg
#' @param optimizer
#' @param loss
#' @param epoch
#' @param batch_size
#' @param verbose
#' @param callbacks
#' @param standardize
#'
#' @return
#' @export
#'
#' @examples
trainNN <- function(presence, bg, conf = NULL, model = NULL, reg = 0,
                    optimizer = "rmsprop", loss = "mse", epoch = 500,
                    batch_size = 128, verbose = 1, callbacks = list(),
                    standardize = TRUE) {

  if (is.null(conf) & is.null(model))
    stop("You must provide conf or model parameter!")
  require(keras)

  x <- rbind(presence@data, bg@data)

  if (standardize) {
    cols <- names(Filter(is.numeric, x))

    means <- apply(x[cols], 2, mean)
    stds <- apply(x[cols], 2, sd)

    x[cols] <- scale(x[cols], center = means, scale = stds)
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
  output <- NN(optimizer = optimizer, loss = loss, model = model, means = means,
               stds = stds)

  return(list(output, history))
}

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
