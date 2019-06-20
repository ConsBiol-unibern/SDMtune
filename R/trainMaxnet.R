#' @importFrom maxnet maxnet maxnet.formula
trainMaxnet <- function(p, a, reg = 1, fc = "lqph") {

  result <- SDMmodel(p = p, a = a)

  x <- rbind(p@data, a@data)
  p <- c(rep(1, nrow(p@data)), rep(0, nrow(a@data)))
  model <- maxnet::maxnet(p, x, f = maxnet::maxnet.formula(p, x, classes = fc),
                          regmult = reg)

  model_object <- Maxnet(reg = reg, fc = fc, model = model)
  result@model <- model_object

  return(result)
}
