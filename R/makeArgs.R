#' Make Arguments
#'
#' Build the arguments used to run a MaxEnt model.
#'
#' @param rm The value of the regularization multiplier.
#' @param fc The value of the feature combination, possible values are combinations of
#' "l", "q", "p", "h" and "t".
#' @param test The path of the test file in a SWD format, if NULL is not used.
#' @param output_format The MaxEnt output format, possible values are "logistic", "cloglog" and "raw"
#' or a shorten version of the previous valus, e.g. "log" for logistic.
#' @param response_curves Flag to compute the response curves, default is FALSE.
#' @param iterations Number of iterations used by the Maxent alghoritm, default is 500.
#' @param extra_args Extra arguments used to run MAxEnt.
#'
#' @return A vactor with the arguments.
#'
#' @examples args <- makeArgs(rm = 2, fc = "LQ")
#'
#' @author Sergio Vignali
makeArgs <- function(rm, fc,
                     output_format = c("logistic", "cloglog", "raw"),
                     test = NULL, response_curves = FALSE, iterations = 500,
                     extra_args = NULL) {

  output_format <- match.arg(output_format)

  args <- c("noaddsamplestobackground", paste0("betamultiplier=", rm),
            paste0("outputformat=", output_format),
            paste0("maximumiterations=", iterations),
            getFeatureArgs(fc))

  if (!is.null(test)) args <- append(args, paste0("testsamplesfile=", test))

  if (response_curves == TRUE) args <- append(args, c("responsecurves=true",
                                                      "writeplotdata=true"))

  if (!is.null(extra_args)) args <- append(args, extra_args)

  return(args)
}
