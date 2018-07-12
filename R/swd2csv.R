#' SWD to csv
#'
#' Save a MaxentSWD object in a csv file.
#'
#' @param swd A MaxentSWD object.
#' @param file_name The name of the file where to save the object.
#'
#' @examples
#' \dontrun{
#' swd2csv(train, "train_data.csv")}
#'
#' @author Sergio Vignali
swd2csv <- function(swd, file_name) {
  df <- cbind(swd@species, swd@coords, swd@data)
  colnames(df)[1] <- "Species"
  write.csv(df, file_name, row.names = FALSE)
}
