trainBRT <- function(data,
                     distribution = "bernoulli",
                     n.trees = 100,
                     interaction.depth = 1,
                     shrinkage = 0.1,
                     bag.fraction = 0.5) {

  result <- SDMmodel(data = data)

  df <- data@data
  df <- cbind(pa = data@pa, df)
  model <- gbm::gbm(pa ~ .,
                    data = df,
                    distribution = distribution,
                    n.trees = n.trees,
                    interaction.depth = interaction.depth,
                    shrinkage = shrinkage,
                    bag.fraction = bag.fraction)

  model_object <- BRT(n.trees = n.trees,
                      distribution = distribution,
                      interaction.depth = interaction.depth,
                      shrinkage = shrinkage,
                      bag.fraction = bag.fraction,
                      model = model)

  result@model <- model_object

  result
}
