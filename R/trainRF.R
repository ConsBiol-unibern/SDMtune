#' @importFrom randomForest randomForest
trainRF <- function(data, mtry = NULL, ntree = 500) {

  result <- SDMmodel(data = data)

  if (is.null(mtry))
    mtry <- floor(sqrt(ncol(data@data)))

  x <- data@data
  p <- data@pa
  model <- randomForest::randomForest(x = x, y = as.factor(p), mtry = mtry,
                                      ntree = ntree)

  model_object <- RF(mtry = model$mtry, ntree = model$ntree, model = model)
  result@model <- model_object

  return(result)
}
