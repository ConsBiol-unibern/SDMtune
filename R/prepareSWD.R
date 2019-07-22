#' Prepare an SWD object
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function creates an \linkS4class{SWD} object (sample with data).
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
#' @return An \linkS4class{SWD} object.
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
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' set.seed(25)
#' bg_coords <- dismo::randomPoints(predictors, 2000)
#'
#' train <- prepareSWD(species = "Vultur gryphus", p = p_coords, a = bg_coords,
#'                     env = predictors, categorical = "biome")
#' presence
prepareSWD <- function(species, p = NULL, a = NULL, coords = NULL, env,
                       categorical = NULL) {

  # TODO Remove it in the next release.
  if (!is.null(coords)) {
    warning("Argument \"coords\" is deprecated and will be removed in the next",
            " release. Please use \"p\" and \"a\" instead.", call. = FALSE)
    coords <- as.data.frame(coords)
    message("Extracting predictor information for given locations...")
    data <- as.data.frame(raster::extract(env, coords))
    # Remove any occurrence point with NA for at least one variable
    index <- complete.cases(data)
    discarded <- nrow(data) - sum(index)
    if (discarded > 0) {
      data <- data[index, ]
      coords <- coords[index, ]
      message("Warning: ", discarded,
              ifelse(discarded == 1, " location is", " locations are"),
              " NA for some environmental variables, ",
              ifelse(discarded == 1, "it ", "they "),
              "will be discard!")
    }
    colnames(coords) <- c("X", "Y")
    # Set categorical variables as factors
    if (!is.null(categorical)) {
      for (i in categorical) {
        data[, i] <- as.factor(data[, i])
      }
    }
    # Reset row names
    rownames(coords) <- NULL
    rownames(data) <- NULL

    swd <- SWD(species = species, coords = coords, data = data,
               pa = NA_real_)
  } else {
    df_coords <- data.frame(X = numeric(), Y = numeric())
    df_data <- p[0, ]
    dfs <- list(p, a)
    text <- c("presence", "absence/background")
    for (i in 1:2) {
      coords <- as.data.frame(dfs[[i]])
      colnames(coords) <- c("X", "Y")
      message("Extracting predictor information for ", text[i], " locations...")
      data <- as.data.frame(raster::extract(env, coords))
      # Remove any occurrence point with NA for at least one variable
      index <- complete.cases(data)
      discarded <- nrow(data) - sum(index)
      if (discarded > 0) {
        data <- data[index, ]
        coords <- coords[index, ]
        message("Warning: ", discarded, " ", text[i],
                ifelse(discarded == 1, " location is", " locations are"),
                " NA for some environmental variables, ",
                ifelse(discarded == 1, "it ", "they "),
                "will be discarded!")
      }
      df_coords <- rbind(df_coords, coords)
      df_data <- rbind(df_data, data)
      if (i == 1) {
        pa <- rep(1, nrow(coords))
      } else {
        pa <- c(pa, rep(0, nrow(coords)))
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
