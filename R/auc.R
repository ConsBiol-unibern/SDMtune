#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula
#'
#' @param model Maxent object.
#' @param presence SWD presence locations, if not provided it computes the train AUC,
#' default is NULL.
#' @param bg SWD backgroung locations used to compute the AUC
#' by the permutation importance function, defaul is NULL.
#'
#' @return The value of the AUC.
#' @export
#'
#' @examples
#' \dontrun{
#' auc(my_model, presence = test_dataset)}
#'
#' @author Sergio Vignali
auc <- function(model, presence = NULL, bg = NULL) {

  if (is.null(presence)) {
    p_pred <- predict(model, model@presence)
  } else {
    p_pred <- predict(model, presence)
  }

  # bg is used for permutation importance
  if (is.null(bg)) {
    bg_pred <- predict(model, model@background)
  } else {
    bg_pred <- predict(model, bg)
  }

  n_pres <- length(p_pred)
  n_bg <- length(bg_pred)

  # AUC using the Mann-Whitney U Test (taken from dismo pkg)
  U <- sum(rank(c(p_pred, bg_pred))[seq(p_pred)]) - (n_pres * (n_pres + 1) / 2)
  auc <- U / (n_bg * n_pres)

  return(auc)
}
