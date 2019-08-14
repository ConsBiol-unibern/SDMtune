#' Train, Validation and Test datasets
#'
#' Split a dataset randomly in training and testing datasets or training,
#' validation and testing datasets.
#'
#' @param x \code{\linkS4class{SWD}} object containing the data that have to be
#' split in training, validation and testing datasets.
#' @param test numeric. The percentage of data withhold for testing.
#' @param val numeric. The percentage of data withhold for validation, default
#' is 0.
#' @param only_presence logical, if \code{TRUE} the split is done only for the
#' presence locations and all the background locations are included in each
#' partition, used manly for presence-only methods, default is \code{FALSE}.
#' @param seed numeric. The value used to set the seed in order to have
#' consistent results, default is \code{NULL}.
#'
#' @details When \code{only_presence = FALSE}, the proportion of presence and
#' absence is preserved.
#'
#' @return A list with the training, validation and testing or training and
#' testing \code{\link{SWD}} objects accordingly.
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
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' # and splitting only the presence locations
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Split presence locations in training (60%), validation (20%) and testing
#' # (20%) datasets and splitting the presence and the absence locations
#' datasets <- trainValTest(data, val = 0.2, test = 0.2)
#' train <- datasets[[1]]
#' val <- datasets[[2]]
#' test <- datasets[[3]]
trainValTest <- function(x, test, val = 0, only_presence = FALSE, seed = NULL) {

  if (class(x) != "SWD")
    stop("x must be an SWD object!")

  if (!is.null(seed))
    set.seed(seed)

  np <- sum(x@pa == 1)
  na <- sum(x@pa == 0)
  p_perm <- sample(which(x@pa == 1))
  a_perm <- sample(which(x@pa == 0))

  if (val > 0) {
    start_p_val <- round(np * (1 - val - test), 0) + 1
    start_a_val <- round(na * (1 - val - test), 0) + 1
    start_p_test <- round(np * val, 0) + start_p_val
    start_a_test <- round(na * val, 0) + start_a_val
    train <- val <- test <- SWD()

    # Training presences
    train@species <- x@species
    train@data <- x@data[p_perm[1:(start_p_val - 1)], ]
    train@coords <- x@coords[p_perm[1:(start_p_val - 1)], ]
    train@pa <- c(rep(1, nrow(train@coords)))

    # Validation presences
    val@species <- x@species
    val@data <- x@data[p_perm[start_p_val:(start_p_test - 1)], ]
    val@coords <- x@coords[p_perm[start_p_val:(start_p_test - 1)], ]
    val@pa <- c(rep(1, nrow(val@coords)))

    # Testind SWD
    test@species <- x@species
    test@data <- x@data[p_perm[start_p_test:np], ]
    test@coords <- x@coords[p_perm[start_p_test:np], ]
    test@pa <- c(rep(1, nrow(test@coords)))
    if (only_presence) {
      # Training absences
      train@data <- rbind(train@data, x@data[x@pa == 0, ])
      train@coords <- rbind(train@coords, x@coords[x@pa == 0, ])
      train@pa <- c(train@pa, rep(0, na))
      # Validation absences
      val@data <- rbind(val@data, x@data[x@pa == 0, ])
      val@coords <- rbind(val@coords, x@coords[x@pa == 0, ])
      val@pa <- c(val@pa, rep(0, na))
      # Testing absences
      test@data <- rbind(test@data, x@data[x@pa == 0, ])
      test@coords <- rbind(test@coords, x@coords[x@pa == 0, ])
      test@pa <- c(test@pa, rep(0, na))
    } else {
      # Training absences
      train@data <- rbind(train@data, x@data[a_perm[1:(start_a_val - 1)], ])
      train@coords <- rbind(train@coords,
                            x@coords[a_perm[1:(start_a_val - 1)], ])
      train@pa <- c(train@pa, rep(0, (nrow(train@coords) - length(train@pa))))
      # Validation absences
      val@data <- rbind(val@data,
                        x@data[a_perm[start_a_val:(start_a_test - 1)], ])
      val@coords <- rbind(val@coords,
                          x@coords[a_perm[start_a_val:(start_a_test - 1)], ])
      val@pa <- c(val@pa, rep(0, (nrow(val@coords) - length(val@pa))))
      # Testing absences
      test@data <- rbind(test@data, x@data[a_perm[start_a_test:na], ])
      test@coords <- rbind(test@coords, x@coords[a_perm[start_a_test:na], ])
      test@pa <- c(test@pa, rep(0, (nrow(test@coords) - length(test@pa))))
    }
    # Reset row names
    rownames(train@data) <- NULL
    rownames(train@coords) <- NULL
    rownames(val@data) <- NULL
    rownames(val@coords) <- NULL
    rownames(test@data) <- NULL
    rownames(test@coords) <- NULL

    return(list(train, val, test))

  } else {
    start_p_test <- round(np * (1 - test), 0) + 1
    start_a_test <- round(na * (1 - test), 0) + 1
    train <- test <- SWD()

    # Training presences
    train@species <- x@species
    train@data <- x@data[p_perm[1:(start_p_test - 1)], ]
    train@coords <- x@coords[p_perm[1:(start_p_test - 1)], ]
    train@pa <- c(rep(1, nrow(train@coords)))

    # Testing presences
    test@species <- x@species
    test@data <- x@data[p_perm[start_p_test:np], ]
    test@coords <- x@coords[p_perm[start_p_test:np], ]
    test@pa <- c(rep(1, nrow(test@coords)))

    if (only_presence) {
      # Training absences
      train@data <- rbind(train@data, x@data[x@pa == 0, ])
      train@coords <- rbind(train@coords, x@coords[x@pa == 0, ])
      train@pa <- c(train@pa, rep(0, na))
      # Testing absences
      test@data <- rbind(test@data, x@data[x@pa == 0, ])
      test@coords <- rbind(test@coords, x@coords[x@pa == 0, ])
      test@pa <- c(test@pa, rep(0, na))
    } else {
      # Training absences
      train@data <- rbind(train@data, x@data[a_perm[1:(start_a_test - 1)], ])
      train@coords <- rbind(train@coords,
                            x@coords[a_perm[1:(start_a_test - 1)], ])
      train@pa <- c(train@pa, rep(0, (nrow(train@coords) - length(train@pa))))
      # Testing absences
      test@data <- rbind(test@data, x@data[a_perm[start_a_test:na], ])
      test@coords <- rbind(test@coords,  x@coords[a_perm[start_a_test:na], ])
      test@pa <- c(test@pa, rep(0, (nrow(test@coords) - length(test@pa))))
    }
    # Reset row names
    rownames(train@data) <- NULL
    rownames(train@coords) <- NULL
    rownames(test@data) <- NULL
    rownames(test@coords) <- NULL

    return(list(train, test))
  }
}
