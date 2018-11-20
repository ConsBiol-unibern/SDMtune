#' Train Neural Network
#'
#' Train a Neural Network model
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param val SWD object with the validation dataset.
#' @param conf list containing vector with layers configuration, see details.
#' @param model a keras model.
#' @param reg numeric. The regularization to be applied to each layer,
#' default is 0.
#' @param optimizer character. The optimizer algorithm, default uses the Adam
#' algorithm.
#' @param loss character. The Loss function to be optimized, default uses the
#' binary crossentropy.
#' @param epoch integer. Number of epoch used to train the model,
#' default is 500.
#' @param batch_size integer. Number of samples used in each mini batch,
#' default is 32.
#' @param verbose integer. Verbosity mode (0 = silent, 1 = progress bar,
#' 2 = text).
#' @param callbacks List of callbacks to be called during training.
#' @param tensorboard logical if TRUE it creates a log directory and it call the
#' tensorboard callback, default is FALSE.
#'
#' @details Write something here...
#'
#' @return NN object.
#' @export
#' @importFrom keras %>% compile fit model_to_yaml tensorboard
#' @importFrom keras callback_early_stopping callback_model_checkpoint
#' @importFrom keras callback_tensorboard
#'
#' @examples \dontrun{
#' trainNN(presence, bg, conf = list(c(100, "tanh"), c(100, "tanh")))}
#'
#' @author Sergio Vignali
trainNN <- function(presence, bg, val = NULL, conf = NULL, model = NULL,
                    reg = 0, optimizer = "adam", loss = "binary_crossentropy",
                    epoch = 500, batch_size = 32, verbose = 1,
                    call_backs = list(), tensorboard = FALSE) {

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

  if (!is.null(val)) {
    val_data <- rbind(val@data, bg@data)
    val_data <- data.matrix(format_data(val_data, means, stds, levels))
    val_data <- list(val_data, c(rep(1, nrow(val@data)), rep(0, nrow(bg@data))))
    pred_callback <- aucsHistory$new(x[1:nrow(presence@data), ],
                                     x[(nrow(presence@data) + 1):nrow(x), ],
                                     val_data[[1]][1:nrow(val@data), ])
  } else {
    val_data <- list()
    pred_callback <- aucsHistory$new(x[1:nrow(presence@data), ],
                                     x[(nrow(presence@data) + 1):nrow(x), ])
  }

  if (tensorboard) {
    dir.create("log")
    keras::tensorboard("log")
    call_backs <- c(call_backs, keras::callback_tensorboard(log_dir = "log"))
  }

  call_backs <- c(call_backs, pred_callback)

  if (!is.null(model)) {
    model <- model
  } else {
    model <- parseNNConf(conf, reg, ncol(x))
  }

  model %>% compile(optimizer = optimizer, loss = loss, metrics = c("acc"))
  yaml <- keras::model_to_yaml(model)

  history <- model %>% fit(x, p, epochs = epoch, batch_size = batch_size,
                           callbacks = call_backs, verbose = verbose,
                           validation_data = val_data)

  model_object <- NN(model = model, history = history, loss = loss,
                     optimizer = optimizer, epoch = epoch,
                     batch_size = batch_size, callbacks = call_backs,
                     min_max = min_max, means = means, stds = stds,
                     levels = levels, yaml = yaml,
                     train_aucs = pred_callback$train_aucs)

  if (!is.null(val))
    model_object@val_aucs <- pred_callback$val_aucs

  result@model <- model_object

  return(result)
}
