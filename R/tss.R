#' True Skill Statistics
#'
#' Compute the max TSS of a given Maxent model.
#'
#' @param model Maxent object.
#' @param test SWD object, if provided it computes the TSS for the test dataset,
#' if not for the train dataset, default is NULL.
#'
#' @return The value of the TSS of the given model.
#' @export
#'
#' @examples \dontrun{
#' tss(model, test = test_dataset)}
#'
#' @author Sergio Vignali
#'
#' @references
#' Allouche O., Tsoar A., Kadmon R., (2006). Assessing the accuracy of species distribution models:
#' prevalence, kappa and the true skill statistic (TSS). Journal of Applied Ecology, 43(6), 1223–1232.
tss <- function(model, test = NULL) {

  cm <- confMatrix(model, test)
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)
  tss <- tpr + tnr - 1

  return(round(max(tss), 4))
}
