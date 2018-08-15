#' Train Maxnet model
#'
#' Train a Maxnet model using the maxnet package.
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param rm numeric. The value of the regularization multiplier.
#' @param type The MaxEnt output type, possible values are "Cloglog", "Logistic",
#' and "Raw", default value "Cloglog".
#' @param test SWD object with the test locations, default is NULL.
#'
#' @return Maxent object.
#' @export
#' @importFrom maxnet maxnet maxnet.formula
#'
#' @examples
#' \dontrun{model <- trainMaxnet(presence, bg, rm)}
#'
#' @author Sergio Vignali
trainMaxnet <- function(presence, bg, rm, fc,
                        type = c("cloglog", "logistic", "raw"), test = NULL) {

  if (class(presence) != "SWD" | class(bg) != "SWD")
    stop("presence and background dataset must be a SWD object!")

  x <- rbind(presence@data, bg@data)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))
  model <- maxnet::maxnet(p, x, f = maxnet::maxnet.formula(p, x, classes = fc),
                          regmult = rm)

  result <- Maxnet(presence = presence, background = bg, rm = rm, fc = fc,
                   type = type, model = model)

  return(result)
}
