#' Print Correlated Variables
#'
#' Utility that prints the name of correlated variables according with the provided
#' method and correlation threashold
#'
#' @param bg SWD. Locations used to test the correlation between environmental variables.
#' @param method character. The method used to comput the correlation matrix, default "spearman".
#' @param cor_th numeric. The correlation threshold used to select highly correlated variables, default is 0.7.
#'
#' @return The name of the correlated variables.
#' @export
#' @importFrom reshape2 melt
#'
#' @examples \dontrun{
#' corVar(bg, method = 'pearson', cor_th = 0.6)}
#'
#' @author Sergio Vignali
corVar <- function(bg, method = "spearman", cor_th = 0.7) {

  if (class(bg) != "SWD")
    stop("bg must be a SWD object!")

  df <- bg@data
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- cor(df, method = method)

  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  cor_matrix[abs(cor_matrix) < cor_th] <- NA
  df <- reshape2::melt(as.matrix(cor_matrix), na.rm = TRUE)
  df <- df[order(-abs(df$value)), ]
  rownames(df) <- NULL

  return(df)
}
