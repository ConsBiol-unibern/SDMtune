#' Train, Validation and Test datasets
#'
#' Split a dataset randomly in training and testing datasets or training,
#' validation and testing datasets.
#'
#' @param x \linkS4class{SWD} object containing the data that have to be split
#' in training, validation and testing datasets.
#' @param test numeric. The percentage of data withhold for testing.
#' @param val numeric. The percentage of data withhold for validation, default
#' is 0.
#' @param seed numeric. The value used to set the seed in order to have
#' consistent results, default is \code{NULL}.
#'
#' @return A list with the training, validation and testing data sets or
#' training and testing data sets accordingly.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (70%) and testing (30%) datasets
#' datasets <- trainValTest(presence, test = 0.3)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Split presence locations in training (60%), validation (20%) and testing
#' # (20%) datasets
#' datasets <- trainValTest(presence, val = 0.2, test = 0.2)
#' train <- datasets[[1]]
#' val <- datasets[[2]]
#' test <- datasets[[3]]
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
