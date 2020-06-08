#' Add Samples to Background
#'
#' The function add the presence locations to the background. This is equivalent
#' to the Maxent argument `addsamplestobackground=true`.
#'
#' @param x \linkS4class{SWD} object.
#' @param all logical, if `TRUE` it adds all the presence locations even if
#' already included in the background locations, default is `FALSE`. This
#' is equivalent to the Maxent argument `addallsamplestobackground=true`.
#'
#' @return An object of class \linkS4class{SWD}.
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
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Add presence locations with values not included in the background to the
#' # background locations
#' new_data <- addSamplesToBg(data)
#' new_data
#'
#' # Add all the presence locations to the background locations, even if they
#' # have values already included in the background
#' new_data <- addSamplesToBg(data, all = TRUE)
#' new_data
addSamplesToBg <- function(x, all = FALSE) {

  if (class(x) != "SWD")
    stop("The function accepts only SWD objects.")

  # Append presence locations after background locations
  o <- x
  o@data <- rbind(x@data, x@data[x@pa == 1, ])
  rownames(o@data) <- NULL
  o@coords <- rbind(x@coords, x@coords[x@pa == 1, ])
  rownames(o@coords) <- NULL
  o@pa <- c(x@pa, rep(0, sum(x@pa)))

  if (!all) {
    # Get index of duplicated background locations
    index <- duplicated(o@data[o@pa == 0, ])
    index <- which(index == TRUE)
    # Get index only for the appended presence locations
    index <- index[index > sum(x@pa == 0)]
    if (length(index) > 0) {
      # Remove duplicates
      o@data <- o@data[-index, ]
      rownames(o@data) <- NULL
      o@coords <- o@coords[-index, ]
      rownames(o@coords) <- NULL
      o@pa <- o@pa[-index]
    }
  }

  return(o)
}
