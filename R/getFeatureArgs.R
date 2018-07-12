#' Get Feature Arguments
#'
#' Given a short word for the feature combination return the corresponding arguments used by MaxEnt.
#'
#' @param fc The value of the feature combination, possible values are combinations of
#' "l", "q", "p", "h" and "t".
#'
#' @return A vector with the arguments.
#'
#' @examples fc_args <- getFeatureArgs('lqp')
#'
#' @author Sergio Vignali
getFeatureArgs <- function(fc) {

  for (letter in strsplit(fc, "")[[1]]) {
    if (!grepl(letter, "lqpht")) {
      stop(paste0("Feature Class '", letter, "' not allawed, possible Feature Classes are: 'l', 'q', 'p', 'h' and 't'!"))
    }
  }

  feature_args <- c("noautofeature")

  if (grepl("l", fc))
    feature_args <- append(feature_args, "linear=true")
  if (grepl("q", fc))
    feature_args <- append(feature_args, "quadratic=true")
  if (grepl("p", fc))
    feature_args <- append(feature_args, "product=true")
  if (grepl("h", fc))
    feature_args <- append(feature_args, "hinge=true")
  if (grepl("t", fc))
    feature_args <- append(feature_args, "threshold=true")

  return(feature_args)
}
