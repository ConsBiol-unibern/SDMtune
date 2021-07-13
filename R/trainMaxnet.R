trainMaxnet <- function(data, reg = 1, fc = "lqph") {

  result <- SDMmodel(data = data)

  x <- data@data
  p <- data@pa
  model <- maxnet::maxnet(p, x, f = maxnet::maxnet.formula(p, x, classes = fc),
                          regmult = reg, addsamplestobackground = FALSE)

  model_object <- Maxnet(reg = reg, fc = fc, model = model)
  result@model <- model_object

  return(result)
}
