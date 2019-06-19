#' SWD to csv
#'
#' Save a \linkS4class{SWD} object as csv file.
#'
#' @param swd \linkS4class{SWD} object.
#' @param file_name character. The name of the file where to save the object.
#'
#' @export
#' @importFrom utils write.csv
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
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
#' # Save the SWD objct as csv file
#' swd2csv(train, "train_data.csv")
#' }
swd2csv <- function(swd, file_name) {
  df <- cbind(swd@species, swd@coords, swd@data)
  colnames(df)[1] <- "Species"
  write.csv(df, file_name, row.names = FALSE)
}
