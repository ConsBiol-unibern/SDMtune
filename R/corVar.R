#' Print Correlated Variables
#'
#' Utility that prints the name of correlated variables and the relative
#' correlation coefficient value.
#'
#' @param bg \link{SWD}. Locations used to test the correlation between
#' environmental variables.
#' @param method character. The method used to compute the correlation matrix,
#' default is "spearman".
#' @param cor_th numeric. If provided it prints only the variables whose
#' correlation coefficient is higher or lower than the given threshold, default
#' is NULL.
#' @param order logical, if TRUE the variable are ordered from the most to the
#' less highly correlated, default is TRUE.
#' @param remove_diagonal logical, if TRUE the values in the diagonal are,
#' removed, default is TRUE.
#'
#' @return The name of the correlated variables.
#' @export
#' @importFrom reshape2 melt
#' @importFrom stats cor
#'
#' @examples \dontrun{
#' corVar(bg, method = 'pearson', cor_th = 0.6)}
#'
#' @author Sergio Vignali
corVar <- function(bg, method = "spearman", cor_th = NULL, order = TRUE,
                   remove_diagonal = TRUE) {

  if (class(bg) != "SWD")
    stop("bg must be a SWD object!")

  df <- bg@data
  # Remove categorical environmental variables
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- cor(df, method = method)
  # Remove lower triangle
  cor_matrix[lower.tri(cor_matrix, diag = remove_diagonal)] <- NA
  if (!is.null(cor_th))
    cor_matrix[abs(cor_matrix) < cor_th] <- NA
  # Convert matrix to long form
  df <- reshape2::melt(as.matrix(cor_matrix), na.rm = TRUE)
  # Strings not as factors
  df[, 1:2] <- lapply(df[, 1:2], as.character)
  if (order)
    df <- df[order(-abs(df$value)), ]
  rownames(df) <- NULL

  return(df)
}
