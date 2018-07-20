#' Evaluate
#'
#' Compute several evaluation metrics
#'
#' @param model Maxent object.
#'
#' @return The TSS.
#' @export
#'
#' @examples
#' \dontrun{
#' eval(model)}
eval <- function(model) {

  presence <- suppressMessages(predict(model, model@presence@data))
  bg <- suppressMessages(predict(model, model@background@data))

  np <- length(presence)
  nb <- length(bg)

  th <- seq(0, 1, length.out = 1000)

  a <- b <- c <- d <- vector(mode = "numeric", length = 1000)

  for (i in 1:length(th)) {
    a[i] <- length(presence[presence >= th[i]])  # true positives
    b[i] <- length(bg[bg >= th[i]])              # false positives
    c[i] <- length(presence[presence < th[i]])   # false negatives
    d[i] <- length(bg[bg < th[i]])               # true negatives
  }

  tpr <- a / (a + c)
  tnr <- d / (b + d)
  fpr <- b / (b + d)
  fnr <- c / (a + c)
  sens <- a / (a + c)
  spec <- d / (b + d)
  tss <- max(sens + spec - 1)

  return(tss)
}
