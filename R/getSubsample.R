#' Get Dataset Subsample
#'
#' Get a random subsample of a MaxentSWD object.
#'
#' @param dataset A MaxentSWD object.
#' @param size The size of the sub sample.
#' @param seed The value used to set the seed in order to have consistent results, default is NULL.
#'
#' @return The sub sample as MaxentSWD object.
#'
#' @examples
#' \dontrun{
#' getSubsample(bg, value = 4500)}
#'
#' @author Sergio Vignali
getSubsample <- function(dataset, size, seed = NULL) {

  if (size > nrow(dataset@data))
    stop(paste(size, "is bigger than dataset observations!"))
  if (class(dataset) != "MaxentSWD")
    stop("Data set must be a MaxentSWD object!")
  if (!is.null(seed))
    set.seed(seed)

  folds <- sample(nrow(dataset@data))
  dataset@data <- dataset@data[folds[1:size], ]
  dataset@coords <- dataset@coords[folds[1:size], ]

  return(dataset)
}
