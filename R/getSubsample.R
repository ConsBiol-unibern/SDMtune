#' Get Dataset Subsample
#'
#' Get a random subsample of an \link{SWD} object.
#'
#' @param dataset \link{SWD} object.
#' @param size numeric. The size of the sub sample.
#' @param seed numeric. The value used to set the seed in order to have
#' consistent results, default is NULL.
#'
#' @return The sub sample as \link{SWD} object.
#' @export
#'
#' @examples
#' \dontrun{
#' getSubsample(data, value = 4500)}
#'
#' @author Sergio Vignali
getSubsample <- function(dataset, size, seed = NULL) {

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
