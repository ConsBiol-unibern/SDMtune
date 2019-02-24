#' Train Maxent model
#'
#' Train a \link{MaxEnt} model using the dismo package. The function runs Maxent
#' model with a minimum setting of optional arguments (e.g. it doesn't make the
#' response curves) to speed up the computation time. Use \link{modelReport}
#' function to set more arguments (i.e. response curves) and save the output
#' permanently in a folder.
#'
#' @param p \link{SWD} object with the presence locations.
#' @param a \link{SWD} object with the background locations.
#' @param reg numeric. The value of the regularization multiplier, default is 1.
#' @param fc vector. The value of the feature combination, possible values are
#' combinations of "l", "q", "p", "h" and "t", default is "lqph".
#' @param iter numeric. Number of iterations used by the Maxent alghoritm,
#' default is 500.
#' @param extra_args vector. Extra arguments used to run MaxEnt, default is
#' "removeduplicates=false".
#'
#'
#' @details By default the function uses
#' **extra_args = "removeduplicates=false"**. In case this is not your expected
#' beaviour you can assign extra_args = "" or you can add any other additional
#' arguments extending the previous vector.
#'
#' @return A \link{SDMmodel} object.
#' @export
#' @importFrom dismo maxent
#' @importFrom utils packageVersion read.csv
#'
#' @examples
#' \dontrun{model <- trainMaxent(p, a)}
#'
#' @author Sergio Vignali
trainMaxent <- function(p, a, reg = 1, fc = "lqph", iter = 500,
                        extra_args = "removeduplicates=false") {

  result <- SDMmodel(p = p, a = a)
  folder <- tempfile()

  args <- .make_args(reg = reg, fc = fc, iter = iter, extra_args = extra_args)

  x <- rbind(p@data, a@data)
  p <- c(rep(1, nrow(p@data)), rep(0, nrow(a@data)))
  dismo_model <- dismo::maxent(x, p, args = args, path = folder)

  l <- .get_lambdas(paste0(folder, "/species.lambdas"), a)
  f <- .formula_from_lambdas(l$lambdas)

  model_object <- Maxent(results = dismo_model@results, reg = reg, fc = fc,
                         iter = iter, extra_args = extra_args,
                         lambdas = dismo_model@lambdas, coeff = l$lambdas,
                         formula = f, lpn = l$lpn, dn = l$dn,
                         entropy = l$entropy, min_max = l$min_max)

  result@model <- model_object

  unlink(folder, recursive = TRUE)

  return(result)
}

.make_args <- function(reg, fc, iter, extra_args) {

  args <- c(paste0("betamultiplier=", reg), paste0("maximumiterations=", iter),
            .get_feature_args(fc))

  args <- c(args, extra_args)

  return(args)
}


.get_feature_args <- function(fc) {

  for (letter in strsplit(fc, "")[[1]]) {
    if (!grepl(letter, "lqpht")) {
      stop(paste0("Feature Class '", letter,
                  "' not allawed, possible Feature Classes are: ",
                  "'l', 'q', 'p', 'h' and 't'!"))
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

.get_lambdas <- function(lambda_file, a) {
  lambdas <- read.csv(lambda_file, header = FALSE)
  lpn <- lambdas[(nrow(lambdas) - 3), 2]
  dn <- lambdas[(nrow(lambdas) - 2), 2]
  entropy <- lambdas[nrow(lambdas), 2]
  lambdas <- as.data.frame(lambdas[1:(nrow(lambdas) - 4), ])
  names(lambdas) <- c("feature", "lambda", "min", "max")
  # Get min max values of variables
  min_max <- lambdas[lambdas$feature %in% names(a@data), ]
  min_max$lambda <- NULL
  names(min_max) <- c("variable", "min", "max")
  # Remove features where lambda = 0 and round braces
  lambdas <- lambdas[lambdas$lambda != 0, ]
  lambdas$feature <- gsub("\\(|\\)", "", lambdas$feature)
  output <- list(lambdas, lpn, dn, entropy, min_max)
  names(output) <- c("lambdas", "lpn", "dn", "entropy", "min_max")
  return(output)
}

.formula_from_lambdas <- function(l) {
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
      fx <- paste0(".product(", vars[[1]][1], ", ", vars[[1]][2], ", ", f$min,
                   ", ", f$max, ")")
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
      fx <- paste0(".categorical(", var_value[[1]][1], ", ",
                   var_value[[1]][2], ")")
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
  (variable ^ 2 - var_min) / (var_max - var_min)
}

.product <- function(var1, var2, var_min, var_max) {
  ( (var1 * var2) - var_min) / (var_max - var_min)
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
