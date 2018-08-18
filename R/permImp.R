#' Permutation Importance
#'
#' The function randomly permutes one variable at time (using train and background
#' datasets) and computes the decrease in training AUC. The result is normalized
#' to percentages. Same implementation of MaxEnt java software but with the additional
#' possibility of running several permutations to obtain a better estimate of the
#' permutation importance. In case of more than one permutation (default is 10)
#' the average of the decrease in training AUC is computed.
#'
#' @param model SDMsel object.
#' @param permut integer. Number of permutations, default is 10.
#'
#' @details Note that it could return values slightly different from MaxEnt java
#' software for a different random permutation.
#'
#' @return data.frame with the ordered permutation importance.
#' @export
#'
#' @examples
#' \dontrun{
#' permImp(model, permut = 50)}
#'
#' @author Sergio Vignali
permImp <- function(model, permut = 10) {

  set.seed(25)
  vars <- colnames(model@presence@data)
  model_auc <- auc(model)
  permutedAUC <- matrix(nrow = permut, ncol = length(vars))

  n_pres <- nrow(model@presence@data)

  for (j in 1:length(vars)) {
    for (i in 1:permut) {
      data <- sample(c(model@presence@data[, vars[j]],
                       model@background@data[, vars[j]]))
      presence_copy <- model@presence
      presence_copy@data[, vars[j]] <- data[1:n_pres]
      bg_copy <- model@background
      bg_copy@data[, vars[j]] <- data[(n_pres + 1):length(data)]
      permutedAUC[i, j] <- auc(model, presence_copy, bg = bg_copy)
    }
  }

  if (permut > 1) {
    sdAUC <- apply(permutedAUC, 2, sd)
    permutedAUC <- apply(permutedAUC, 2, mean)
  }

  perm_imp <- pmax(0, (model_auc - permutedAUC))
  perm_imp <- 100 * perm_imp / sum(perm_imp)
  perm_imp <- round(perm_imp, 1)

  output <- data.frame(Variable = vars, Permutation_importance = perm_imp)
  if (permut > 1)
    output$sd <- round(sdAUC, 3)
  output <- output[order(output$Permutation_importance, decreasing = TRUE), ]
  row.names(output) <- NULL

  return(output)
}
