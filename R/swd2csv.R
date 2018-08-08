#' SWD to csv
#'
#' Save a MaxentSWD object in a csv file.
#'
#' @param swd SWD object.
#' @param file_name character. The name of the file where to save the object.
#'
#' @export
#' @importFrom utils write.csv
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
