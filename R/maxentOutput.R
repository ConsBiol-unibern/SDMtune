#' MaxEnt Output
#'
#' Given a model the function produces the output in the given folder.
#'
#' @param model Maxent object.
#' @param response_curves logical, if TRUE it plots the response curves in the html output,
#' default is FALSE.
#' @param jk logical, if TRUE it runs the jackknife test, default FALSE.
#' @param threads numeric. Number of threads used by MaxEnt to compute the
#' Jackknife test, default is 1.
#' @param folder character. The name of the folder in which to save the MaxEnt output,
#' default is NULL meaning a temporary folder. The folder is created in the working directory.
#'
#' @export
#'
#' @examples
#' \dontrun{model <- produceOutput(model, response_curves = T, jacckinfe = T,
#' thread = 4, folder = 'my_folder'))}
#'
#' @author Sergio Vignali
maxentOutput <- function(model, response_curves = FALSE, jk = FALSE,
                          threads = 1, folder = NULL) {

  train <- model@presence
  bg <- model@background
  rm <- model@rm
  fc <- model@fc
  type <- model@type
  iter <- model@iter
  if (nrow(model@test@data) == 0) {
    test <- NULL
  } else {
    test <- model@test
  }

  extra_args <- c(paste0("threads=", threads))
  if (response_curves)
    extra_args <- c(extra_args, "responsecurves=true", "writeplotdata=true")
  if (jk)
    extra_args <- c(extra_args, "jackknife=true")

  output <- trainMaxent(train, bg, rm, fc, test = test, type = type,
                         iter = iter, folder = folder, extra_args = extra_args)

  return(invisible(output))

}
