#' Model Output
#'
#' Given a model the function produces the output in the given folder.
#'
#' @param model SDMmodel object.
#' @param type character MaxEnt output type, possible values are "cloglog",
#' "logistic" and "raw", default is "cloglog".
#' @param folder character. The name of the folder in which to save the MaxEnt
#' output. The folder is created in the working directory.
#' @param test SWD object with the test locations, default is NULL.
#' @param response_curves logical, if TRUE it plots the response curves in the
#' html output, default is FALSE.
#' @param jk logical, if TRUE it runs the jackknife test, default FALSE.
#' @param threads numeric. Number of threads used by MaxEnt to compute the
#' Jackknife test, default is 1.
#'
#' @return The SDMmodel object.
#' @export
#'
#' @examples
#' \dontrun{model <- modelOutput(model, type = "logistic", test = test,
#' response_curves = T, jk = T, thread = 4, folder = "my_folder"))}
#'
#' @author Sergio Vignali
modelOutput <- function(model, type = c("cloglog", "logistic", "raw"), folder,
                        test = NULL, response_curves = FALSE, jk = FALSE,
                        threads = 1) {

  if (class(model@model) != "Maxent")
    stop("modelOutput function works only with Maxent model for the moment!")

  dir.create(folder)
  extra_args <- model@model@extra_args

  if (!is.null(test)) {
    test@data <- test@data[colnames(model@presence@data)]
    test_file <- paste0(folder, "/test.csv")
    test@species <- "species"
    swd2csv(test, test_file)
    extra_args <- c(extra_args, paste0("testsamplesfile=", test_file))
  }

  type <- match.arg(type)

  extra_args <- c(extra_args, paste0("threads=", threads),
                  paste0('outputformat=', type))
  if (response_curves)
    extra_args <- c(extra_args, "responsecurves=true", "writeplotdata=true")
  if (jk)
    extra_args <- c(extra_args, "jackknife=true")

  extra_args <- c(extra_args, paste0("folder=", folder))

  model <- trainMaxent(presence = model@presence, bg = model@background,
                       reg = model@model@reg, fc = model@model@fc,
                       iter = model@model@iter, extra_args = extra_args)

  if (!is.null(test)) {
    test@species <- model@presence@species
  }

  gc()

  return(invisible(model))

}
