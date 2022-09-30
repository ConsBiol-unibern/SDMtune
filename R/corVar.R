#' Print Correlated Variables
#'
#' Utility that prints the name of correlated variables and the relative
#' correlation coefficient value.
#'
#' @param bg \linkS4class{SWD} object with the locations used to compute the
#' correlation between environmental variables.
#' @param method character. The method used to compute the correlation matrix.
#' @param cor_th numeric. If provided it prints only the variables whose
#' correlation coefficient is higher or lower than the given threshold.
#' @param order logical. If `TRUE` the variable are ordered from the most to the
#' less highly correlated.
#' @param remove_diagonal logical. If `TRUE` the values in the diagonal are
#' removed.
#'
#' @return A data.frame with the variables and their correlation.
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
#' # Prepare background locations
#' bg_coords <- terra::spatSample(predictors,
#'                                size = 10000,
#'                                method = "random",
#'                                na.rm = TRUE,
#'                                xy = TRUE,
#'                                values = FALSE)
#'
#' # Create SWD object
#' bg <- prepareSWD(species = "Virtual species",
#'                  a = bg_coords,
#'                  env = predictors,
#'                  categorical = "biome")
#'
#' # Get the correlation among all the environmental variables
#' corVar(bg,
#'        method = "spearman")
#'
#' # Get the environmental variables that have a correlation greater or equal to
#' # the given threshold
#' corVar(bg,
#'        method = "pearson",
#'        cor_th = 0.8)
corVar <- function(bg,
                   method = "spearman",
                   cor_th = NULL,
                   order = TRUE,
                   remove_diagonal = TRUE) {

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    cli::cli_abort(
      "Please install package {.pkg reshape2} to use this function",
      call = NULL
    )
  }

  if (!inherits(bg, "SWD"))
    cli::cli_abort(c(
      "!" = "{.var bg} must be an {.cls SWD} object",
      "x" = "You have supplied a {.cls {class(bg)}} instead."
    ))

  df <- bg@data
  # Remove categorical environmental variables
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- stats::cor(df, method = method)
  # Remove lower triangle
  cor_matrix[lower.tri(cor_matrix, diag = remove_diagonal)] <- NA
  if (!is.null(cor_th))
    cor_matrix[abs(cor_matrix) < cor_th] <- NA
  # Convert matrix to long form
  df <- reshape2::melt(as.matrix(cor_matrix), na.rm = TRUE)
  if (order)
    df <- df[order(-abs(df$value)), ]
  rownames(df) <- NULL

  df
}
