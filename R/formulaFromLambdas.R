#' Formula From Lambdas
#'
#' Create the formula to predict Maxent models starting from the lambdas object.
#' Used internally to create the MaxentModel object.
#'
#' @param l The lambdas parameters.
#'
#' @return The formula to predict Maxent models.
#'
#' @examples \dontrun{
#' formulaFromLambdas(lambda)}
#'
#' @author Sergio Vignali
formulaFromLambdas <- function(l) {
  #l <- model@lambdas
  fxs <- vector()
  for (i in 1:nrow(l)) {
    f <- l[i, ]
    # Quadratic
    if (grepl("\\^", f$feature)) {
      var <- sub("\\^2", "", f$feature)
      fx <- paste0("quadratic(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Product
    else if (grepl("\\*", f$feature)) {
      vars <- strsplit(f$feature, "\\*")
      fx <- paste0("product(", vars[[1]][1], ", ", vars[[1]][2], ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Hinge
    else if (grepl("\\'", f$feature)) {
      var <- sub("\\'", "", f$feature)
      fx <- paste0("hinge(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Reverse hinge
    else if (grepl("\\`", f$feature)) {
      var <- sub("\\`", "", f$feature)
      fx <- paste0("revHinge(", var, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
    # Threshold
    else if (grepl("<", f$feature)) {
      vars <- strsplit(f$feature, "<")
      fx <- paste0("threshold(", vars[[1]][2], ", ", vars[[1]][1], ")")
      fxs <- append(fxs, fx)
    }
    # Categorical
    else if (grepl("=", f$feature)) {
      var_value <- strsplit(f$feature, "=")
      fx <- paste0("categorical(", var_value[[1]][1], ", ", var_value[[1]][2], ")")
      fxs <- append(fxs, fx)
    }
    # Linear
    else {
      fx <- paste0("linear(", f$feature, ", ", f$min, ", ", f$max, ")")
      fxs <- append(fxs, fx)
    }
  }
  f <- formula(paste("~", paste(fxs, collapse = " + "), "- 1"))

  return(f)
}

linear <- function(variable, var_min, var_max) {
  (variable - var_min) / (var_max - var_min)
}

quadratic <- function(variable, var_min, var_max) {
  (variable^2 - var_min) / (var_max - var_min)
}

product <- function(var1, var2, var_min, var_max) {
  ((var1 * var2) - var_min) / (var_max - var_min)
}

hinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_min, 0, (variable - var_min) / (var_max - var_min))
}

revHinge <- function(variable, var_min, var_max) {
  ifelse(variable <= var_max, (var_max -  variable) / (var_max - var_min), 0)
}

threshold <- function(variable, th) {
  ifelse(variable <= th, 0, 1)
}

categorical <- function(variable, category) {
  ifelse(variable == category, 1, 0)
}
