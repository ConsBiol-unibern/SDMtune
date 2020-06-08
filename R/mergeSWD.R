#' Merge SWD Objects
#'
#' Merge two \linkS4class{SWD} objects.
#'
#' @param swd1 \linkS4class{SWD} object.
#' @param swd2 \linkS4class{SWD} object.
#' @param only_presence logical, if `TRUE` only for the presence locations are
#' merged and the absence/background locations are taken only from the `swd1`
#' object, default is `FALSE`.
#'
#' @details
#' * In case the two \linkS4class{SWD} objects have different columns, only the
#' common columns are used in the merged object.
#' * The \linkS4class{SWD} object is created in a way that the presence
#' locations are always before than the absence/background locations.
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
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split only presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Merge the training and the testing datasets together
#' merged <- mergeSWD(train, test, only_presence = TRUE)
#'
#' # Split presence and absence locations in training (80%) and testing (20%)
#' datasets
#' datasets <- trainValTest(data, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Merge the training and the testing datasets together
#' merged <- mergeSWD(train, test)
mergeSWD <- function(swd1, swd2, only_presence = FALSE) {

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

  if (only_presence) {
    # Align only presence data
    swd@data <- rbind(swd1@data[swd1@pa == 1, ], swd2@data[swd2@pa == 1, ],
                      swd1@data[swd1@pa == 0, ])
    rownames(swd@data) <- NULL
    # Align only presence coords
    swd@coords <- rbind(swd1@coords[swd1@pa == 1, ],
                        swd2@coords[swd2@pa == 1, ],
                        swd1@coords[swd1@pa == 0, ])
    rownames(swd@coords) <- NULL

    # Align pa
    swd@pa <- c(swd1@pa[swd1@pa == 1], swd2@pa[swd2@pa == 1],
                swd1@pa[swd1@pa == 0])
  } else {
    # Align presence/absence data
    swd@data <- rbind(swd1@data[swd1@pa == 1, ], swd2@data[swd2@pa == 1, ],
                      swd1@data[swd1@pa == 0, ], swd2@data[swd2@pa == 0, ])
    rownames(swd@data) <- NULL
    # Align presence/absence coords
    swd@coords <- rbind(swd1@coords[swd1@pa == 1, ],
                        swd2@coords[swd2@pa == 1, ],
                        swd1@coords[swd1@pa == 0, ],
                        swd2@coords[swd2@pa == 0, ])
    rownames(swd@coords) <- NULL

    # Align pa
    swd@pa <- c(swd1@pa[swd1@pa == 1], swd2@pa[swd2@pa == 1],
                swd1@pa[swd1@pa == 0], swd2@pa[swd2@pa == 0])
  }

  return(swd)
}
