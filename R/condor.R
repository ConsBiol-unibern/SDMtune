#' Locations of Vultur gryphus.
#'
#' The dataset contains 509 locations of the Andean condor collected from 2008
#' and 2018. The locations are downloaded from the Global Biodiversity Inventory
#' Facility and filtered to remove douplicates and keep only one location per
#' raster cell of the World Clime dataset.
#'
#' @format A data frame with 509 rows and 4 variables:
#' \describe{
#'   \item{x}{Longitude of the observation}
#'   \item{y}{Latitude of the observation}
#'   \item{key}{Key of the single observation}
#'   \item{datasetKey}{Key of the corresponding dataset}
#' }
#' @source \url{http://www.gbif.org/}
#'
#' @references
#' Levatich T, Padilla F (2017). EOD - eBird Observation Dataset. Cornell Lab of
#' Ornithology. Occurrence dataset \url{https://doi.org/10.15468/aomfnb}
#' accessed via GBIF.org on 2019-01-13.
#'
#' iNaturalist.org (2018). iNaturalist Research-grade Observations. Occurrence
#' dataset \url{https://doi.org/10.15468/ab3s5x} accessed via GBIF.org on
#' 2019-01-13.
#'
#' naturgucker.de. naturgucker. Occurrence dataset
#' \url{https://doi.org/10.15468/uc1apo} accessed via GBIF.org on 2019-01-13.
"condor"
