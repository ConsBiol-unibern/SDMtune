#' Get Lambdas
#'
#' Get lambdas object from **.lambdas** file, Used internally to create the MaxentModel object.
#'
#' @param lambda_file The path of the **.lambdas** file.
#' @param bg The background locations used to train the model, given as MaxentSWD object.
#'
#' @return The data fetched from the **.lambdas** file.
#'
#' @examples \dontrun{
#' getLambdas("species.lambdas"), bg)}
#'
#' @author Sergio Vignali
getLambdas <- function(lambda_file, bg) {
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
