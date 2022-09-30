#' Prepare an SWD object
#'
#' Given the coordinates, the species' name and the environmental variables,
#' the function creates an \linkS4class{SWD} object (sample with data).
#'
#' @param species character. The name of the species.
#' @param env \link[terra]{rast} containing the environmental variables used to
#' extract the values at coordinate locations.
#' @param p data.frame. The coordinates of the presence locations.
#' @param a data.frame. The coordinates of the absence/background locations.
#' @param categorical vector indicating which of the environmental variable are
#' categorical.
#' @param verbose logical, if `TRUE` prints informative messages.
#'
#' @details The \linkS4class{SWD} object is created in a way that the presence
#' locations are always before than the absence/background locations.
#'
#' @return An \linkS4class{SWD} object.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create the SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#' data
prepareSWD <- function(species,
                       env,
                       p = NULL,
                       a = NULL,
                       categorical = NULL,
                       verbose = TRUE) {

  # TODO: Remove with version 2.0.0
  if (inherits(env, "Raster")) {
    .warn_raster("raster", "rast")
    env <- terra::rast(env)
  }

  if (!inherits(env, "SpatRaster"))
    cli::cli_abort(c(
      "!" = "{.var env} must be a {.cls SpatRaster} object",
      "x" = "You have supplied a {.cls {class(env)}} instead."
    ))

  df_coords <- data.frame(X = numeric(), Y = numeric())
  df_data <- p[0, ]
  dfs <- list(p, a)
  text <- c("presence", "absence/background")
  values <- c(1, 0)
  pa <- c()

  for (i in 1:2) {

    if (!is.null(dfs[[i]])) {

      if (verbose)
        cli::cli_progress_step(
          "Extracting predictor information for {.field {text[i]}} locations"
        )

      coords <- as.data.frame(dfs[[i]])
      colnames(coords) <- c("X", "Y")
      data <- terra::extract(env,
                             coords,
                             ID = FALSE)

      # Remove any occurrence point with NA for at least one variable
      index <- stats::complete.cases(data)
      discarded <- nrow(data) - sum(index)

      if (discarded > 0) {
        data <- data[index, ]
        coords <- coords[index, ]
        cli::cli_warn(c(
          "!" = paste("{.val {discarded}} location{?s} {?is/are} NA for some",
                      "environmental variables and {?has/have} been discarded")
        ))
      }

      df_coords <- rbind(df_coords, coords)
      df_data <- rbind(df_data, data)
      pa <- c(pa, rep(values[i], nrow(coords)))

      if (verbose)
        cli::cli_progress_done()
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

  SWD(species = species,
      coords = df_coords,
      data = df_data,
      pa = pa)

}
