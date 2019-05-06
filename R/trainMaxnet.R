#' Train Maxnet model
#'
#' Train a \link{Maxnet} model using the \link{maxnet} package.
#'
#' @param p \link{SWD} object with the presence locations.
#' @param a \link{SWD} object with the background locations.
#' @param reg numeric. The value of the regularization intensity, default is 1.
#' @param fc vector. The value of the feature classes, possible values are
#' combinations of "l", "q", "p", "h" and "t", default is "lqph".
#'
#' @return A \link{SDMmodel} object.
#' @export
#' @importFrom maxnet maxnet maxnet.formula
#'
#' @examples
#' \dontrun{model <- trainMaxnet(p, a)}
#'
#' @author Sergio Vignali
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
