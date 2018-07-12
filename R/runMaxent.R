#' Maxent Model
#'
#' This Class represents a MaxEnt model objects and hosts all the information about the model.
#'
#' @slot presence MaxentSWD. The presence locations used to train the model.
#' @slot background MaxentSWD. The backgorund locations used to train the model.
#' @slot test MaxentSWD. The test locations used to validate the model.
#' @slot results matrix. The result that usually MaxEnt provide as a csv file.
#' @slot rm numeric. The value of the regularization multiplier used to train the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot iterations numeric. The number of iterations used to train the model.
#' @slot output_format character. The output format of the model.
#' @slot coeff data.frame. The lambda coefficients of the model.
#' @slot formula formula. The formula used to make prediction.
#' @slot lpn numeric. Linear Predictor Normalizer.
#' @slot dn numeric. Density Normalizer.
#' @slot entropy numeric. The entropy value.
#' @slot min_max data.frame. The minimum and maximum values of the continuous variables, used for clamping.
#' @slot plot_data list. If available a list containing the value to plot the response curves.
#' @slot path character. The path where to save the fold with the files produced by MaxEnt,
#' available if the "folder" parameter is provided to the runMaxent function.
#' @slot html character. The path of the html file produced by MaxEnt, available if the "folder"
#' parameter is provided to the runMaxent function.
#'
#' @author Sergio Vignali
MaxentModel <- setClass("MaxentModel",
                        slots = c(
                          presence = "MaxentSWD",
                          background = "MaxentSWD",
                          test = "MaxentSWD",
                          results = "matrix",
                          rm = "numeric",
                          fc = "character",
                          iterations = "numeric",
                          output_format = "character",
                          coeff = "data.frame",
                          formula = "formula",
                          lpn = "numeric",
                          dn = "numeric",
                          entropy = "numeric",
                          min_max = "data.frame",
                          plot_data = "list",
                          path = "character",
                          html = "character")
)

setMethod("show",
          signature = "MaxentModel",
          definition = function(object) {
            cat("Class                    :", class(object), "\n")
            cat("Species                  :", object@presence@species, "\n")
            cat("Regularization multiplier:", object@rm, "\n")
            cat("Feature Class            :", object@fc, "\n")
            cat("Iterations               :", object@iterations, "\n")
            cat("Output Format            :", object@output_format, "\n")
            cat("Presence locations       :", nrow(object@presence@data), "\n")
            cat("Background locations     :", nrow(object@background@data), "\n")
            cat("Test locations           :", nrow(object@test@data), "\n")
            cat("Continuous variables     :", names(Filter(is.numeric, object@presence@data)), "\n")
            cat("Categoricals             :", names(Filter(is.factor, object@presence@data)), "\n")
            cat("Plot data                :", ifelse(identical(object@plot_data, list()), "No", "Yes"))

            if (file.exists(object@html)) browseURL(object@html)
          })

#' Run MaxEnt
#'
#' Run a MaxEnt model using the dismo package.
#'
#' @param train The data frame used to train the model given as MaxentSWD object.
#' @param bg The data frame with the background locations given as MaxentSWD object.
#' @param rm The value of the regularization multiplier.
#' @param fc The value of the feature combination, possible values are combinations of
#' "L", "Q", "P", "H" and "T".
#' @param test The test dataset given as MaxentSWD object, default is NULL.
#' @param jackknife Flag to activate the jackknife test, default FALSE.
#' @param output_format The MaxEnt output format, possible values are "Logistic", "Cloglog",
#' "Cumulative" and "Raw", default value "Cloglog".
#' @param response_curves Flag to compute the response curves, default is FALSE.
#' @param iterations Number of iterations used by the Maxent alghoritm, default is 500.
#' @param threads Number of threads used by MaxEnt, default is 1.
#' @param extra_args Extra arguments used to run MaxEnt, e.g. "removeduplicates=false", default
#' is NULL.
#' @param folder The folder name where to save the MaxEnt output, default is NULL meaning a
#' temporary folder. The folder is created in the working directory.
#'
#' @return The output of MaxEnt as MaxentModel object.
#'
#' @examples
#' \dontrun{model <- runMaxent(train, bg, rm, jacckinfe = TRUE, response_curves = TRUE))}
#'
#' @author Sergio Vignali
runMaxent <- function(train, bg, rm, fc, test = NULL, output_format = "cloglog",
                      response_curves = FALSE, iterations = 500,
                      extra_args = NULL, folder = NULL) {

  if (class(train) != "MaxentSWD" | class(bg) != "MaxentSWD")
    stop("Train and background dataset must be a MaxentSWD object!")

  delete_folder <- FALSE

  if (is.null(folder)) {
    folder <- tempfile()
    delete_folder = TRUE
  } else {
    dir.create(folder)
  }

  if (is.null(test)) {
    test_file = NULL
  } else {
    if (class(test) != "MaxentSWD")
      stop("Test dataset must be a MaxentSWD object!")
    if (delete_folder == TRUE) {
      test_folder = tempfile()
      dir.create(test_folder)
      test_file <- paste0(test_folder, "/test.csv")
    } else {
      test_file <- paste0(folder, "/test.csv")
    }
    test@species <- "species"
    swd2csv(test, test_file)
  }

  plot_data <- list()
  args <- makeArgs(rm = rm, fc = fc, test = test_file,
                   output_format = output_format,
                   response_curves = response_curves,
                   iterations = iterations, extra_args = extra_args)

  x <- rbind(train@data, bg@data)
  p <- c(rep(1, nrow(train@data)), rep(0, nrow(bg@data)))
  model <- maxent(x, p, args = args, path = folder)

  if (response_curves == TRUE) {
    path <- paste0(folder, "/plots/")
    files <- dir(path, pattern = ".dat")
    for (i in 1 : length(files)) {
      plot_data[[i]] <- read.csv(paste0(path, files[i]))
    }
    names <- gsub(".dat", "", files)
    names <- gsub("species_", "", names)
    names(plot_data) <- names
  }

  l <- getLambdas(paste0(folder, "/species.lambdas"), bg)
  f <- formulaFromLambdas(l$lambdas)

  result <- MaxentModel(presence = train, background = bg,
                        results = model@results, rm = rm, fc = fc,
                        iterations = iterations, output_format = output_format,
                        coeff = l$lambdas, formula = f, lpn = l$lpn, dn = l$dn,
                        entropy = l$entropy, min_max = l$min_max,
                        plot_data = plot_data)

  if (!is.null(test)) {
    test@species <- train@species
    result@test <- test
  }

  if (delete_folder == TRUE) {
    unlink(folder, recursive = TRUE)
    if (!is.null(test))
      unlink(test_folder, recursive = TRUE)
    result@path <- ""
    result@html <- ""
  } else {
    result@path <- paste0(getwd(), "/", folder)

    output_file <- paste0(result@path, "/species.html")
    f <- readLines(output_file)
    f[1] <- paste0("<title>", train@species, "</title>")
    f[2] <- paste0("<center><h1>Maxent model for ", train@species, "</h1></center>")
    f[3] <- paste("<br><center><b>Output produced using 'SDMSelection' version", packageVersion("SDMSelection"),
                     "(Vignali S. <i>et al.</i>, 2018) and 'dismo' version", packageVersion("dismo"),
                     "(Hijmans R. J. <i>et al.</i>, 2017).</b></center><br>", f[3])
    f[length(f) + 1] <- "<br><hr><br>"
    f[length(f) + 1] <- "- Sergio Vignali, Arnaud Barras and Veronika Braunisch (2018). SDMSelection: Species Distribution Model Selection. R package version 0.1.0."
    f[length(f) + 1] <- '<br>- Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. <a href="http://CRAN.R-project.org/package=dismo" target="_blank">CRAN</<a>'
    writeLines(f, output_file)
    result@html <- output_file
    file.remove(model@html)
  }

  return(result)
}
