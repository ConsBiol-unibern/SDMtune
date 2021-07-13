trainMaxent <- function(data, reg = 1, fc = "lqph", iter = 500) {

  extra_args <- c("removeduplicates=false", "addsamplestobackground=false")
  result <- SDMmodel(data = data)
  folder <- tempfile()

  args <- .make_args(reg = reg, fc = fc, iter = iter, extra_args = extra_args)

  x <- data@data
  p <- data@pa
  dismo_model <- dismo::maxent(x, p, args = args, path = folder, silent = TRUE)

  l <- .get_lambdas(dismo_model@lambdas)
  f <- stats::formula(paste("~", paste(l$lambdas$feature, collapse = " + "),
                            "- 1"))

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
            .get_fc_args(fc))

  args <- c(args, extra_args)

  return(args)
}


.get_fc_args <- function(fc) {

  # Check if fc includes characters different from lqpht
  if (grepl("[^lqpht]", fc))
    stop(paste(fc, "feature classes not allowed, possible values are 'lqpht'!"))

  feature_args <- c("noautofeature")

  # Add threshold feature class if included
  if (grepl("t", fc))
    feature_args <- c(feature_args, "threshold")

  # Remove not included feature classes
  no_fc <- Reduce(setdiff, strsplit(c("lqph", gsub("t", "", fc)), ""))

  for (letter in no_fc) {
    feature_args <- c(feature_args, get("fc_map")[[letter]])
  }

  return(feature_args)
}

.get_lambdas <- function(lambda) {
  l <- as.data.frame(stringr::str_split(lambda, ", ", simplify = TRUE),
                     stringsAsFactors = FALSE)
  l[, 2:4] <- sapply(l[, 2:4], as.numeric)
  n_row <- nrow(l)
  lpn <- l[(n_row - 3), 2]
  dn <- l[(n_row - 2), 2]
  entropy <- l[n_row, 2]
  l <- l[1:(n_row - 4), ]
  names(l) <- c("feature", "lambda", "min", "max")

  # Create feature
  l$feature <- gsub("\\((.*)=(.*)\\)", ".categorical\\(\\1, \\2\\)", l$feature)
  l$feature <- gsub("\\((.*)<(.*)\\)", ".threshold\\(\\2, \\1\\)", l$feature)
  l$feature <- gsub("(.*)\\^2", "I\\(\\1\\^2\\)", l$feature)
  l$feature <- gsub("(.*)\\*(.*)", "I\\(\\1*\\2\\)", l$feature)
  l$feature <- ifelse(grepl("'", l$feature),
                      paste0(".hinge(", sub("'", "", l$feature), ", ", l$min,
                             ", ", l$max, ")"),
                      l$feature)
  l$feature <- ifelse(grepl("`", l$feature),
                      paste0(".rev_hinge(", sub("`", "", l$feature), ", ",
                             l$min, ", ", l$max, ")"),
                      l$feature)

  # Get min max values of variables
  min_max <- l[!grepl("[(]", l$feature), ]
  min_max$lambda <- NULL
  names(min_max) <- c("variable", "min", "max")

  # Remove features where lambda = 0 and round braces
  l <- l[l$lambda != 0, ]
  # Reset row names
  rownames(l) <- NULL
  rownames(min_max) <- NULL

  output <- list(lambdas = l, lpn = lpn, dn = dn, entropy = entropy,
                 min_max = min_max)
  return(output)
}

.hinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_min, 0, (variable - var_min) / (var_max - var_min))
}

.rev_hinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_max, (var_max -  variable) / (var_max - var_min), 0)
}

.threshold <- function(variable, th) {
  ifelse(variable < th, 0, 1)
}

.categorical <- function(variable, category) {
  ifelse(variable == category, 1, 0)
}
