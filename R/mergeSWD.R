#' Merge SWD Objects
#'
#' Merge two \linkS4class{SWD} object.
#'
#' @param swd1 \linkS4class{SWD} object.
#' @param swd2 \linkS4class{SWD} object.
#'
#' @details In case the two \linkS4class{SWD} objects have different columns,
#' only the common columns are used in the merged object.
#'
#' @return The merged \linkS4class{SWD} object.
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Merge the training and the testing datasets together
#' merged <- mergeSWD(train, test)
mergeSWD <- function(swd1, swd2) {

  if (class(swd1) != "SWD" | class(swd2) != "SWD")
    stop("The function accepts only SWD objects!")

  if (swd1@species != swd2@species)
    stop("SWD1 and SWS2 have a different spwcies!")

  if (length(colnames(swd1@data)) != length(colnames(swd2@data)) ||
      length(intersect(colnames(swd1@data), colnames(swd2@data))) !=
      length(colnames(swd1@data))) {
    warning(paste("The two SWD objects have different columns,",
                  "only the common columns are used in the merged object!"))
    # Get common variables
    vars <- intersect(colnames(swd1@data), colnames(swd2@data))
    # Subset objects
    swd1@data <- swd1@data[, vars]
    swd2@data <- swd2@data[, vars]
  }

  swd <- new("SWD")
  swd@species <- swd1@species
  swd@coords <- rbind(swd1@coords, swd2@coords)
  swd@data <- rbind(swd1@data, swd2@data)
  return(swd)
}
