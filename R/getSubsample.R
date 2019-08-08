#' Get Dataset Subsample, Deprecated
#'
#' Get a random subsample of an \linkS4class{SWD} object. This function is
#' deprecated, don't use it with the an \code{\link{SWD}} object that contains
#' presence and absence/background locations together.
#'
#' @param dataset \linkS4class{SWD} object.
#' @param size numeric. The size of the sub sample.
#' @param seed numeric. The value used to set the seed in order to have
#' consistent results, default is \code{NULL}.
#'
#' @return The sub sample as \linkS4class{SWD} object.
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
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' bg <- prepareSWD(species = "Virtual species", a = bg_coords, env = predictors,
#'                  categorical = "biome")
#'
#' # Get a subsample
#' getSubsample(bg, size = 2000, seed = 25)
getSubsample <- function(dataset, size, seed = NULL) {

  .Deprecated(msg = "This function is deprecated and will be removed in the next release. Don't use it SWD objects containing presence and absence/backaground data!")

  if (class(dataset) != "SWD")
    stop("Data set must be an SWD object!")
  if (size > nrow(dataset@data))
    stop(paste(size, "is bigger than dataset observations!"))
  if (!is.null(seed))
    set.seed(seed)

  folds <- sample(nrow(dataset@data))
  dataset@data <- dataset@data[folds[1:size], ]
  dataset@coords <- dataset@coords[folds[1:size], ]

  # Reset row names
  rownames(dataset@data) <- NULL
  rownames(dataset@coords) <- NULL

  return(dataset)
}
