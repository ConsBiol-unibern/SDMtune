#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param rm numeric. The value of the regularization multiplier.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t".
#' @param iter numeric. Number of iterations used by the Maxent alghoritm,
#' default is 500, used only for "Maxent" method.
#' @param extra_args vector. Extra arguments used to run MaxEnt, defalt is
#' c("noaddsamplestobackground", "removeduplicates=false"), used only for
#' "Maxent" method.#'
#'
#' @details See \link{trainMaxent} and \link{trainMaxnet} for details related to
#' the different methods. For **Maxent** models the function uses by default
#' **extra_args = c("noaddsamplestobackground", "removeduplicates=false")**.
#' In case this is not your expected beaviour you can remove both passing
#' **extra_args = ""** or you can add any other additional arguments extending
#' the previous vector.
#'
#' @return A SWDmodel object
#' @export
#'
#' @examples \dontrun{
#' model <- train("Maxnet", presence, bg, rm = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
train <- function(method = c("Maxent", "Maxnet"), presence, bg, rm, fc,
                  iter = 500, extra_args = c("noaddsamplestobackground",
                                             "removeduplicates=false")) {
  method = match.arg(method)

  if (method == "Maxent") {
    model <- trainMaxent(presence = presence, bg = bg, rm = rm, fc = fc,
                         iter = iter, extra_args = extra_args)
  } else {
    model <- trainMaxnet(presence = presence, bg = bg, rm = rm, fc = fc)
  }

  return(model)
}
