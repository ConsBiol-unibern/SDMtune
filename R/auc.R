#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula
#'
#' @param model SDMmodel object.
#' @param test SWD test locations, if not provided it computes the train AUC,
#' default is NULL.
#' @param bg SWD backgroung locations used to compute the AUC
#' by the permutation importance function, defaul is NULL.
#'
#' @return The value of the AUC.
#' @export
#'
#' @examples
#' \dontrun{
#' auc(my_model, test = test_dataset)}
#'
#' @author Sergio Vignali
auc <- function(model, test = NULL, bg = NULL) {

  if (class(model@model) != "Maxnet") {
    object <- model@model
  } else {
    object <- model@model@model
  }

  if (is.null(test)) {
    p_pred <- predict(object, model@presence@data)
  } else {
    p_pred <- predict(object, test@data)
  }

  # bg is used for permutation importance
  if (is.null(bg)) {
    bg_pred <- predict(object, model@background@data)
  } else {
    bg_pred <- predict(object, bg@data)
  }

  n_pres <- length(p_pred)
  n_bg <- length(bg_pred)

  # AUC using the Mann-Whitney U Test (taken from dismo pkg)
  U <- sum(rank(c(p_pred, bg_pred))[seq(p_pred)]) - (n_pres * (n_pres + 1) / 2)
  auc <- U / (n_bg * n_pres)

  return(round(auc, 4))
}
