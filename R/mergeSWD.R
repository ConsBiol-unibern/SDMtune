#' Merge SWD Objects
#'
#' @param swd1 SWD object.
#' @param swd2 SWD object.
#'
#' @return The merged SWD object.
#' @export
#'
#' @examples\dontrun{
#' mergeSWD(train, background)}
#'
#' @author Sergio Vignali
mergeSWD <- function(swd1, swd2) {

  if (class(swd1) != "SWD" | class(swd2) != "SWD")
    stop("The function accepts only SWD objects!")

  if (swd1@species != swd2@species)
    stop("SWD1 and SWS2 have a different spwcies!")

  swd <- new("SWD")
  swd@species <- swd1@species
  swd@coords <- rbind(swd1@coords, swd2@coords)
  swd@data <- rbind(swd1@data, swd2@data)
  return(swd)
}
