#' Train Maxent model
#'
#' Train a MaxEnt model using the dismo package.
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param rm numeric. The value of the regularization multiplier.
#' @param fc vector. The value of the feature combination, possible values are combinations of
#' "l", "q", "p", "h" and "t".
#' @param type The MaxEnt output type, possible values are "Cloglog", "Logistic",
#' "Cumulative" and "Raw", default value "Cloglog".
#' @param test SWD object with the test locations, default is NULL.
#' @param iter numeric. Number of iterations used by the Maxent alghoritm, default is 500.
#' @param extra_args vector. Extra arguments used to run MaxEnt, e.g. "removeduplicates=false", default
#' is NULL.
#' @param folder character. The folder name where to save the MaxEnt output, default is NULL meaning
#' that is not saved. The folder is created in the working directory.
#'
#' @return The output of MaxEnt as Maxent object.
#' @export
#' @importFrom dismo maxent
#' @importFrom utils packageVersion read.csv
#'
#' @examples
#' \dontrun{model <- trainMaxent(presence, bg, rm)}
#'
#' @author Sergio Vignali
trainMaxent <- function(presence, bg, rm, fc,
                        type = c("cloglog", "logistic", "raw"), test = NULL,
                        iter = 500, extra_args = NULL, folder = NULL) {

  if (class(presence) != "SWD" | class(bg) != "SWD")
    stop("presence and background dataset must be a SWD object!")

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
    if (class(test) != "SWD")
      stop("Test dataset must be a SWD object!")
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

  type <- match.arg(type)

  args <- .makeArgs(rm = rm, fc = fc, type = type, test = test_file,
                    iter = iter, extra_args = extra_args)

  x <- rbind(presence@data, bg@data)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))
  model <- dismo::maxent(x, p, args = args, path = folder)

  l <- .getLambdas(paste0(folder, "/species.lambdas"), bg)
  f <- .formulaFromLambdas(l$lambdas)

  result <- Maxent(presence = presence, background = bg,
                   results = model@results, rm = rm, fc = fc,
                   iter = iter, type = type,
                   lambdas = model@lambdas, coeff = l$lambdas, formula = f,
                   lpn = l$lpn, dn = l$dn, entropy = l$entropy,
                   min_max = l$min_max)

  if (!is.null(test)) {
    test@species <- presence@species
    result@test <- test
  }

  if (delete_folder == TRUE) {
    unlink(folder, recursive = TRUE)
    if (!is.null(test))
      unlink(test_folder, recursive = TRUE)
    result@folder <- ""
  } else {
    result@folder <- paste0(getwd(), "/", folder)

    output_file <- paste0(result@folder, "/species.html")
    species <- gsub(" ", "_", tolower(result@presence@species))
    f <- readLines(output_file)
    f[1] <- paste0("<title>", presence@species, "</title>")
    f[2] <- paste0("<center><h1>Maxent model for ", presence@species, "</h1></center>")
    f[3] <- paste("<br><center><b>Output produced using 'SDMsel' version", packageVersion("SDMsel"),
                     "(Vignali S. <i>et al.</i>, 2018) and 'dismo' version", packageVersion("dismo"),
                     "(Hijmans R. J. <i>et al.</i>, 2017).</b></center><br>", f[3])
    f[length(f) + 1] <- "<br><hr><br>"
    f[length(f) + 1] <- "- Sergio Vignali, Arnaud Barras and Veronika Braunisch (2018). SDMsel: Species Distribution Model Selection. R package version 0.1.0."
    f[length(f) + 1] <- '<br>- Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. <a href="http://CRAN.R-project.org/package=dismo" target="_blank">CRAN</<a>'
    writeLines(f, output_file)
    file.remove(model@html)
    # Rename files
    for (file in list.files(path = result@folder, pattern = "species*",
                            full.names = TRUE))
      file.rename(file, sub("species", species, file))
  }

  return(result)
}

.makeArgs <- function(rm, fc, type, test, iter, extra_args) {

  args <- c("noaddsamplestobackground", paste0("betamultiplier=", rm),
            paste0("maximumiterations=", iter),
            paste0('outputformat=', type), .getFeatureArgs(fc))

  if (!is.null(test)) args <- append(args, paste0("testsamplesfile=", test))

  if (!is.null(extra_args)) args <- append(args, extra_args)

  return(args)
}


.getFeatureArgs <- function(fc) {

  for (letter in strsplit(fc, "")[[1]]) {
    if (!grepl(letter, "lqpht")) {
      stop(paste0("Feature Class '", letter, "' not allawed, possible Feature Classes are: 'l', 'q', 'p', 'h' and 't'!"))
    }
  }

  feature_args <- c("noautofeature")

  if (!grepl("l", fc))
    feature_args <- append(feature_args, "nolinear")
  if (!grepl("q", fc))
    feature_args <- append(feature_args, "noquadratic")
  if (!grepl("p", fc))
    feature_args <- append(feature_args, "noproduct")
  if (!grepl("h", fc))
    feature_args <- append(feature_args, "nohinge")
  if (grepl("t", fc))
    feature_args <- append(feature_args, "threshold=true")

  return(feature_args)
}

.getLambdas <- function(lambda_file, bg) {
  lambdas <- read.csv(lambda_file, header = FALSE)
  lpn <- lambdas[(nrow(lambdas) - 3), 2]
  dn <- lambdas[(nrow(lambdas) - 2), 2]
  entropy = lambdas[nrow(lambdas), 2]
  lambdas <- as.data.frame(lambdas[1:(nrow(lambdas) - 4), ])
  names(lambdas) <- c("feature", "lambda", "min", "max")
  # Get min max values of variables
  min_max <- lambdas[lambdas$feature %in% names(bg@data), ]
  min_max$lambda <- NULL
  names(min_max) <- c("variable", "min", "max")
  # Remove features where lambda = 0 and round braces
  lambdas <- lambdas[lambdas$lambda != 0, ]
  lambdas$feature <- gsub("\\(|\\)", "", lambdas$feature)
  output <- list(lambdas, lpn, dn, entropy, min_max)
  names(output) <- c("lambdas", "lpn", "dn", "entropy", "min_max")
  return(output)
}

.formulaFromLambdas <- function(l) {
  fxs <- vector()
  for (i in 1:nrow(l)) {
    f <- l[i, ]
    # Quadratic
    if (grepl("\\^", f$feature)) {
      var <- sub("\\^2", "", f$feature)
      fx <- paste0(".quadratic(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Product
    else if (grepl("\\*", f$feature)) {
      vars <- strsplit(f$feature, "\\*")
      fx <- paste0(".product(", vars[[1]][1], ", ", vars[[1]][2], ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Hinge
    else if (grepl("\\'", f$feature)) {
      var <- sub("\\'", "", f$feature)
      fx <- paste0(".hinge(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Reverse hinge
    else if (grepl("\\`", f$feature)) {
      var <- sub("\\`", "", f$feature)
      fx <- paste0(".revHinge(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Threshold
    else if (grepl("<", f$feature)) {
      vars <- strsplit(f$feature, "<")
      fx <- paste0(".threshold(", vars[[1]][2], ", ", vars[[1]][1], ")")
      fxs <- append(fxs, fx)
    }
    # Categorical
    else if (grepl("=", f$feature)) {
      var_value <- strsplit(f$feature, "=")
      fx <- paste0(".categorical(", var_value[[1]][1], ", ", var_value[[1]][2], ")")
      fxs <- append(fxs, fx)
    }
    # Linear
    else {
      fx <- paste0(".linear(", f$feature, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
  }
  f <- formula(paste("~", paste(fxs, collapse = " + "), "- 1"))

  return(f)
}

.linear <- function(variable, var_min, var_max) {
  (variable - var_min) / (var_max - var_min)
}

.quadratic <- function(variable, var_min, var_max) {
  (variable^2 - var_min) / (var_max - var_min)
}

.product <- function(var1, var2, var_min, var_max) {
  ((var1 * var2) - var_min) / (var_max - var_min)
}

.hinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_min, 0, (variable - var_min) / (var_max - var_min))
}

.revHinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_max, (var_max -  variable) / (var_max - var_min), 0)
}

.threshold <- function(variable, th) {
  ifelse(variable <= th, 0, 1)
}

.categorical <- function(variable, category) {
  ifelse(variable == category, 1, 0)
}
