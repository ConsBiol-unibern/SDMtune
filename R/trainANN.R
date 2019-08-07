#' @importFrom nnet nnet
#' @importFrom utils capture.output
trainANN <- function(data, size, decay = 0, rang = 0.7, maxit = 100) {

  result <- SDMmodel(data = data)

  x <- data@data
  p <- data@pa
  capture.output(model <- nnet::nnet(x = x, y = p, size = size, decay = decay,
                                     rang = rang, maxit = maxit))

  model_object <- ANN(size = size, decay = decay, rang = rang, maxit = maxit,
                      model = model)
  result@model <- model_object

  return(result)
}
