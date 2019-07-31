#' Prepare an SWD object
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function creates an \code{\linkS4class{SWD}} object (sample with data).
#'
#' @param species character. The name of the species.
#' @param coords Deprecated.
#' @param p data.frame. The coordinates of the presence locations.
#' @param a data.frame. The coordinates of the absence/background locations.
#' @param env \code{\link[raster]{stack}} containing the environmental variables
#' used to extract the values at coordinate locations.
#' @param categorical vector indicating which of the environmental variable are
#' categorical, default is \code{NULL}.
#'
#' @details The \code{\linkS4class{SWD}} object is created in a way that the
#' presence locations are always before than the absence/background locations.
#'
#' @return An \code{\linkS4class{SWD}} object.
#' @export
#' @importFrom stats complete.cases
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
#' # Create the SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#' data
prepareSWD <- function(species, p = NULL, a = NULL, coords = NULL, env,
                       categorical = NULL) {

  # TODO Remove it in the next release.
  if (!is.null(coords)) {
    stop("Argument \"coords\" is deprecated. Please use \"p\" and \"a\" ",
         "instead.")
  } else {
    df_coords <- data.frame(X = numeric(), Y = numeric())
    df_data <- p[0, ]
    dfs <- list(p, a)
    text <- c("presence", "absence/background")
    values <- c(1, 0)
    pa <- c()
    for (i in 1:2) {
      if (!is.null(dfs[[i]])) {
        coords <- as.data.frame(dfs[[i]])
        colnames(coords) <- c("X", "Y")
        message("Extracting predictor information for ", text[i],
                " locations...")
        data <- as.data.frame(raster::extract(env, coords))
        # Remove any occurrence point with NA for at least one variable
        index <- complete.cases(data)
        discarded <- nrow(data) - sum(index)
        if (discarded > 0) {
          data <- data[index, ]
          coords <- coords[index, ]
          message("Info: ", discarded, " ", text[i],
                  ifelse(discarded == 1, " location is", " locations are"),
                  " NA for some environmental variables, ",
                  ifelse(discarded == 1, "it is ", "they are "),
                  "discarded!")
        }
        df_coords <- rbind(df_coords, coords)
        df_data <- rbind(df_data, data)
        pa <- c(pa, rep(values[i], nrow(coords)))
      }
    }
    # Set categorical variables as factors
    if (!is.null(categorical)) {
      for (i in categorical) {
        df_data[, i] <- as.factor(df_data[, i])
      }
    }
    # Reset row names
    rownames(df_coords) <- NULL
    rownames(df_data) <- NULL

    swd <- SWD(species = species, coords = df_coords, data = df_data, pa = pa)
  }

  return(swd)
}
