#' Maxent Variable Importance
#'
#' Shows the percent contribution and permutation importance of the variable used to train the model.
#'
#' @param model SDMmodel object.
#'
#' @return A data frame with the variable importance.
#' @export
#'
#' @examples
#' \dontrun{
#' varContribution(model)}
#'
#' @author Sergio Vignali
maxentVarImp <- function(model) {

  if (class(model@model) != "Maxent")
    stop("Model must be trained using Maxent!")

  input <- model@model@results
  pc <- input[grepl("contribution", rownames(input)), ]
  pi <- input[grepl("permutation.importance", rownames(input)), ]
  variables <- gsub(".contribution", "", names(pc))
  df <- data.frame(Variable = variables, Permutation_importance = round(pi, 1),
                   Percent_contribution = round(pc, 1),
                   row.names = NULL, stringsAsFactors = FALSE)

  df <- df[order(-df$Permutation_importance), ]
  row.names(df) <- NULL
  return(df)
}
