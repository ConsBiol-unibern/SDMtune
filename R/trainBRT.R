#' @importFrom gbm gbm
trainBRT <- function(data, distribution = "bernoulli", ntree = 100,
                     interaction.depth = 1, lr = 0.1, bag.fraction = 0.5) {

  result <- SDMmodel(data = data)

  df <- data@data
  df <- cbind(pa = data@pa, df)
  model <- gbm::gbm(pa ~ ., data = df, distribution = distribution,
                    n.trees = ntree, interaction.depth = interaction.depth,
                    shrinkage = lr, bag.fraction = bag.fraction)

  model_object <- BRT(ntree = ntree, distribution = distribution,
                      interaction.depth = interaction.depth, lr = lr,
                      bag.fraction = bag.fraction, model = model)
  result@model <- model_object

  return(result)
}
