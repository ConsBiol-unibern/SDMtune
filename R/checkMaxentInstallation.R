#' Check Maxent Installation
#'
#' The function checks if Maxent is correctly installed.
#'
#' @param verbose logical, if `TRUE` the function provides useful messages to
#' understand what is not correctly installed, default is `TRUE`.
#'
#' @details In order to have Maxent correctly configured is necessary that:
#' * Java is installed;
#' * the package "rJava" is installed;
#' * the file "maxent.jar" is in the correct folder.
#'
#' @return `TRUE` if Maxent is correctly installed, `FALSE` otherwise.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' checkMaxentInstallation()
checkMaxentInstallation <- function(verbose = TRUE) {

  is_ok <- TRUE

  # Check if Java is installed
  x <- suppressWarnings(system2(command = "java", args = "-version",
                                stdout = FALSE, stderr = FALSE))
  if (x != 0) {
    is_ok <- FALSE
    if (verbose)
      message("Java is not installed.")
  } else if (verbose) {
    message("Java is installed.")
  }

  # Check if the pkg rJava is installed
  if (!requireNamespace("rJava", quietly = TRUE)) {
    is_ok <- FALSE
    if (verbose)
      message("The packege \"rJava\" is not installed.")
  } else if (verbose) {
    message("The packege \"rJava\" is installed.")
  }

  # Check if the file maxent.jar is in the correct folder
  if (!file.exists(file.path(system.file(package = "dismo"),
                             "java", "maxent.jar"))) {
    is_ok <- FALSE
    if (verbose)
      message("The file \"maxent.jar\" is not present in the correct folder.")
  } else if (verbose) {
    message("The file \"maxent.jar\" is present in the correct folder.")
  }

  return(is_ok)
}
