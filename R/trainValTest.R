#' Train, Validation and Test datasets
#'
#' Split a dataset randomly in train and test datasets or train, validation and
#' test datasets.
#'
#' @param x SWD object containing the data that have to be splitted in train,
#' validation and test datasets.
#' @param test numeric. The percentage of data withhold for testing.
#' @param val numeric. The percentage of data withhold for validation, default
#' is 0.
#' @param seed numeric. The value used to set the seed in order to have
#' consistent results, default is NULL.
#'
#' @return A list with the train, validation and test data sets or train and
#' test sets accordingly.
#' @export
#'
#' @examples
#' \dontrun{
#' sets <- trainValTest(presence, val = 0.2, test = 0.2, set_seed = 25)}
#'
#' @author Sergio Vignali
trainValTest <- function(x, test, val = 0, seed = NULL) {

  if (class(x) != "SWD")
    stop("x must be an SWD object!")

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(x@data)
  permutation <- sample(n)

  if (val > 0) {
    start_val <- round(n * (1 - val - test), 0) + 1
    start_test <- round(n * val, 0) + start_val
    train <- val <- test <- SWD()
    train@data <- x@data[permutation[1:(start_val - 1)], ]
    train@coords <- x@coords[permutation[1:(start_val - 1)], ]
    train@species <- x@species
    # Reset row names
    rownames(train@data) <- NULL
    rownames(train@coords) <- NULL
    val@data <- x@data[permutation[start_val:(start_test - 1)], ]
    val@coords <- x@coords[permutation[start_val:(start_test - 1)], ]
    val@species <- x@species
    # Reset row names
    rownames(val@data) <- NULL
    rownames(val@coords) <- NULL
    test@data <- x@data[permutation[start_test:n], ]
    test@coords <- x@coords[permutation[start_test:n], ]
    test@species <- x@species
    # Reset row names
    rownames(test@data) <- NULL
    rownames(test@coords) <- NULL

    return(list(train, val, test))

  } else {
    start_test <- round(n * (1 - test), 0) + 1
    train <- test <- SWD()
    train@data <- x@data[permutation[1:(start_test - 1)], ]
    train@coords <- x@coords[permutation[1:(start_test - 1)], ]
    train@species <- x@species
    # Reset row names
    rownames(train@data) <- NULL
    rownames(train@coords) <- NULL
    test@data <- x@data[permutation[start_test:n], ]
    test@coords <- x@coords[permutation[start_test:n], ]
    test@species <- x@species
    # Reset row names
    rownames(test@data) <- NULL
    rownames(test@coords) <- NULL

    return(list(train, test))
  }
}
