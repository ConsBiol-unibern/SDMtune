#' Train Maxnet model
#'
#' Train a Maxnet model using the maxnet package.
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param reg numeric. The value of the regularization intensity, default is 1.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t", default is "lqph".
#'
#' @return A SDMmodel object.
#' @export
#' @importFrom maxnet maxnet maxnet.formula
#'
#' @examples
#' \dontrun{model <- trainMaxnet(presence, bg)}
#'
#' @author Sergio Vignali
trainMaxnet <- function(presence, bg, reg = 1, fc = "lqph") {

  result <- SDMmodel(presence = presence, background = bg)

  x <- rbind(presence@data, bg@data)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))
  model <- maxnet::maxnet(p, x, f = maxnet::maxnet.formula(p, x, classes = fc),
                          regmult = reg)

  model_object <- Maxnet(reg = reg, fc = fc, model = model)
  result@model <- model_object

  return(result)
}
