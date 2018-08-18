#' Model Output
#'
#' Given a model the function produces the output in the given folder.
#'
#' @param model Maxent object.
#' @param type character MaxEnt output type, possible values are "cloglog",
#' "logistic" and "raw", default is "cloglog".
#' @param response_curves logical, if TRUE it plots the response curves in the
#' html output, default is FALSE.
#' @param jk logical, if TRUE it runs the jackknife test, default FALSE.
#' @param threads numeric. Number of threads used by MaxEnt to compute the
#' Jackknife test, default is 1.
#' @param folder character. The name of the folder in which to save the MaxEnt
#' output, default is NULL meaning a temporary folder. The folder is created in
#' the working directory.
#'
#' @return The model object.
#' @export
#'
#' @examples
#' \dontrun{model <- modelOutput(model, type = "logistic", response_curves = T,
#' jk = T, thread = 4, folder = "my_folder"))}
#'
#' @author Sergio Vignali
modelOutput <- function(model, type = c("cloglog", "logistic", "raw"),
                        response_curves = FALSE, jk = FALSE, threads = 1,
                        folder = NULL) {

  type <- match.arg(type)
  if (nrow(model@test@data) == 0) {
    test <- NULL
  } else {
    test <- model@test
  }

  extra_args <- c(paste0("threads=", threads), paste0('outputformat=', type))
  if (response_curves)
    extra_args <- c(extra_args, "responsecurves=true", "writeplotdata=true")
  if (jk)
    extra_args <- c(extra_args, "jackknife=true")

  output <- trainMaxent(presence = model@presence, bg = model@background,
                        rm = model@model@rm, fc = model@model@fc, test = test,
                        iter = model@model@iter, folder = folder,
                        extra_args = extra_args)

  gc()

  return(invisible(output))

}
