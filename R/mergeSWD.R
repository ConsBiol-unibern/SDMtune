#' Merge SWD Objects
#'
#' Merge two \link{SWD} object.
#'
#' @param swd1 \link{SWD} object.
#' @param swd2 \link{SWD} object.
#'
#' @details In case the two \link{SWD} objects have different columns, only the
#' common columns are used in the merged object.
#'
#' @return The merged \link{SWD} object.
#' @export
#'
#' @examples\dontrun{
#' mergeSWD(train, val)}
#'
#' @author Sergio Vignali
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
