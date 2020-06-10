#' SWD to csv
#'
#' Save an \linkS4class{SWD} object as csv file.
#'
#' @param swd \linkS4class{SWD} object.
#' @param file_name character. The name of the file in which to save the object,
#' see details.
#'
#' @details
#' * The `file_name` argument should include the extension (i.e. my_file.csv).
#' * If `file_name` is a single name the function saves the presence
#' absence/background locations in a single file, adding the column **pa** with
#' 1s for presence and 0s for absence/background locations. If `file_name` is a
#' vector with two names, it saves the object in two files: the first name
#' is used for the presence locations and the second for the absence/background
#' locations.
#'
#' @export
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
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#' \dontrun{
#' # The following commands save the output in the working directory
#' # Save the SWD object as a single csv file
#' swd2csv(data, "train_data.csv")
#'
#' # Save the SWD object in two separate csv files
#' swd2csv(data, c("presence.csv", "absence.csv"))
#' }
#' }
swd2csv <- function(swd, file_name) {

  if (length(file_name) == 2) {
    df <- cbind(swd@species, swd@coords, swd@data)
    colnames(df)[1] <- "Species"
    utils::write.csv(df[swd@pa == 1, ], file_name[1], row.names = FALSE)
    utils::write.csv(df[swd@pa == 0, ], file_name[2], row.names = FALSE)
  } else {
    df <- cbind(swd@species, swd@pa, swd@coords, swd@data)
    colnames(df)[1:2] <- c("Species", "pa")
    utils::write.csv(df, file_name[1], row.names = FALSE)
  }
}
