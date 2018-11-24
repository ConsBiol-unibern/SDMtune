#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet".
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param reg numeric. The value of the regularization intensiy, default is 1.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t", default is "lqph".
#' @param ... Argument passed to the relative functions, see \link{trainMaxent}
#' or \link{trainMaxnet} for details related to the different methods.
#'
#' @return A SWDmodel object
#' @export
#'
#' @examples \dontrun{
#' model <- train("Maxnet", presence, bg, reg = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
train <- function(method = c("Maxent", "Maxnet"), presence, bg, reg = 1,
                  fc = "lqph", ...) {
  method = match.arg(method)

  if (method == "Maxent") {
    model <- trainMaxent(presence = presence, bg = bg, reg = reg, fc = fc, ...)
  } else {
    model <- trainMaxnet(presence = presence, bg = bg, reg = reg, fc = fc)
  }

  return(model)
}
