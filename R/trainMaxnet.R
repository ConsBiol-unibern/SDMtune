#' Train Maxnet model
#'
#' Train a Maxnet model using the maxnet package.
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param rm numeric. The value of the regularization multiplier.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t".
#'
#' @return A SDMmodel object.
#' @export
#' @importFrom maxnet maxnet maxnet.formula
#'
#' @examples
#' \dontrun{model <- trainMaxnet(presence, bg, rm)}
#'
#' @author Sergio Vignali
trainMaxnet <- function(presence, bg, rm, fc) {

  result <- SDMmodel(presence = presence, background = bg)

  x <- rbind(presence@data, bg@data)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))
  model <- maxnet::maxnet(p, x, f = maxnet::maxnet.formula(p, x, classes = fc),
                          regmult = rm)

  model_object <- Maxnet(rm = rm, fc = fc, model = model)
  result@model <- model_object

  return(result)
}
