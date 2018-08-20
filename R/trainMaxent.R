#' Train Maxent model
#'
#' Train a MaxEnt model using the dismo package. The function runs Maxent model
#' with a minimum settings of optional parameters to speed the computation time.
#' Use \link{modelOutput} function to set more parameters (i.e. response curves)
#' and save the output permanently in a folder.
#'
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param rm numeric. The value of the regularization multiplier.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t".
#' @param iter numeric. Number of iterations used by the Maxent alghoritm,
#' default is 500.
#' @param extra_args vector. Extra arguments used to run MaxEnt, defalt is
#' c("noaddsamplestobackground", "removeduplicates=false").
#'
#'
#' @details The function by default **uses extra_args =
#' c("noaddsamplestobackground", "removeduplicates=false")**. In case this is
#' not your expected beaviour you can remove both passing extra_args = "" or you
#' ca add any other additional arguments extending the previous vector.
#'
#' @return A SDMmodel object.
#' @export
#' @importFrom dismo maxent
#' @importFrom utils packageVersion read.csv
#'
#' @examples
#' \dontrun{model <- trainMaxent(presence, bg, rm,
#' extra_args = )}
#'
#' @author Sergio Vignali
trainMaxent <- function(presence, bg, rm, fc, iter = 500,
                        extra_args = c("noaddsamplestobackground",
                                       "removeduplicates=false")) {

  result <- SDMmodel(presence = presence, background = bg)

  if (paste(extra_args, collapse = "") != "" &
      grepl("folder=", extra_args[length(extra_args)])) {
      delete_folder <- FALSE
      folder <- sub("folder=", "", extra_args[length(extra_args)])
      extra_args <- extra_args[-length(extra_args)]
  } else {
    delete_folder <- TRUE
    folder <- tempfile()
  }

  args <- .makeArgs(rm = rm, fc = fc, iter = iter, extra_args = extra_args)

  x <- rbind(presence@data, bg@data)
  p <- c(rep(1, nrow(presence@data)), rep(0, nrow(bg@data)))
  dismo_model <- dismo::maxent(x, p, args = args, path = folder)

  l <- .getLambdas(paste0(folder, "/species.lambdas"), bg)
  f <- .formulaFromLambdas(l$lambdas)

  model_object <- Maxent(results = dismo_model@results, rm = rm, fc = fc,
                         iter = iter, lambdas = dismo_model@lambdas,
                         coeff = l$lambdas, formula = f, lpn = l$lpn, dn = l$dn,
                         entropy = l$entropy, min_max = l$min_max)

  result@model <- model_object

  if (delete_folder == TRUE) {
    unlink(folder, recursive = TRUE)
    result@model@folder <- ""
  } else {
    result@model@folder <- paste0(getwd(), "/", folder)

    output_file <- paste0(result@model@folder, "/species.html")
    species <- gsub(" ", "_", tolower(presence@species))
    f <- readLines(output_file)
    f[1] <- paste0("<title>", presence@species, "</title>")
    f[2] <- paste0("<center><h1>Maxent model for ", presence@species, "</h1></center>")
    f[3] <- paste("<br><center><b>Output produced using 'SDMsel' version", packageVersion("SDMsel"),
                     "(Vignali S. <i>et al.</i>, 2018) and 'dismo' version", packageVersion("dismo"),
                     "(Hijmans R. J. <i>et al.</i>, 2017).</b></center><br>", f[3])
    f[length(f) + 1] <- "<br><hr><br>"
    f[length(f) + 1] <- "- Sergio Vignali, Arnaud Barras and Veronika Braunisch (2018). SDMsel: Species Distribution Model Selection. R package version 0.1.0."
    f[length(f) + 1] <- '<br>- Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. <a href="http://CRAN.R-project.org/package=dismo" target="_blank">CRAN</<a>'
    f <- gsub("species_", paste0(species, "_"), f)
    f <- gsub("species.csv", paste0(species, ".csv"), f)
    f <- gsub("species.lambdas", paste0(species, ".lambdas"), f)
    writeLines(f, output_file)
    file.remove(dismo_model@html)
    # Rename files
    for (file in list.files(path = result@model@folder, pattern = "species*",
                            full.names = TRUE, recursive = TRUE))
      file.rename(file, sub("species", species, file))
  }

  gc()
  return(result)
}

.makeArgs <- function(rm, fc, iter, extra_args) {

  args <- c(paste0("betamultiplier=", rm), paste0("maximumiterations=", iter),
            .getFeatureArgs(fc))

  args <- c(args, extra_args)

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
