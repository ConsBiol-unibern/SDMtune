#' Variable Selection
#'
#' The function performs a data driven variable selection. Starting from provided model
#' it iterates through the .......
#'
#' @param model A MaxentModel object.
#' @param bg4cor Background locations used to test the correlation between environmental variables,
#' given as MaxentSWD object.
#' @param metric character. The metric used to evaluate the models, possible values are:
#' "auc", "tss" and "aicc", default is "auc".
#' @param env \link{stack} or \link{brick} containing the environmental variables,
#' used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#' @param rm integer. The value of the regularization paramiter to use during computation,
#' default is 0.001, see details.
#' @param method The method used to comput the correlation matrix, default "spearman".
#' @param cor_th The correlation threshold used to select highly correlated variables, default is 0.7.
#' @param use_permutation Flag to select the permutation importance or the percent contribution.
#'
#' @details You need package \pkg{snow} to use parallel computation and \pkg{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
#' We should write something more... I will refer to our paper for the explanations...
#'
#' @return The name of the selected variables.
#' @export
#' @importFrom progress progress_bar
#'
#' @examples \dontrun{
#' varSel(model, bg, use_permutation = T)}
#'
#' @author Sergio Vignali
varSel <- function(model, bg4cor, metric = c("auc", "tss", "aicc"), env = NULL,
                   parallel = FALSE, rm = 0.001, method = "spearman",
                   cor_th = 0.7, use_permutation = TRUE) {

  if (class(bg4cor) != "SWD")
    stop("bg4cort must be a SWD object!")

  cor_vars <- corVar(bg4cor, method = method, cor_th = cor_th)
  cor_vars <- unique(c(as.character(cor_vars$Var1),
                       as.character(cor_vars$Var2)))
  total <- length(cor_vars)
  change_rm = FALSE
  if (rm != model@rm) {
    total <- total + 2
    change_rm = TRUE
  }
  removed <- 0

  pb <- progress::progress_bar$new(
    format = "Var Selection [:bar] :percent in :elapsedfull", total = total,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  metric <- match.arg(metric)
  correlation_removed <- FALSE
  vars <- colnames(model@presence@data)

  if (nrow(model@test@data) == 0) {
    test <- NULL
  } else {
    test <- model@test
  }

  if (change_rm) {
    old_rm <- model@rm
    model <- trainMaxent(model@presence, model@background, rm, model@fc,
                         type = model@type, test = test, iter = model@iter)
    pb$tick(1)
  }

  # Remove categorical environmental variables
  df <- bg4cor@data
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- cor(df, method = method)

  while (correlation_removed == FALSE) {

    cor_matrix <- as.data.frame(cor_matrix)
    scores <- varImp(model)
    if (use_permutation)
      scores <- scores[order(-scores$Permutation_importance), ]
    varnames <- scores$Variable
    discarded_variable <- NULL

    for (i in 1:length(varnames)) {

      if (varnames[i] %in% categorical)
        next

      coeff <- cor_matrix[varnames[i]]
      hcv <- row.names(coeff)[abs(coeff) >= cor_th]

      if (length(hcv) > 1) {
        jk_test <- suppressMessages(doJk(model, metric = metric,
                                         variables = hcv, with_only = FALSE,
                                         env = env, parallel = parallel,
                                         return_models = TRUE))
        if (metric != "aicc") {
          index <- which.max(jk_test$results[, 2])
        } else {
          index <- which.min(jk_test$results[, 2])
        }

        model <- jk_test$models_without[[index]]
        discarded_variable <- as.character(jk_test$results$Variable[index])
        cor_matrix[discarded_variable] <- NULL
        cor_matrix <- cor_matrix[!(row.names(cor_matrix) == discarded_variable), ]
        removed <- removed + 1
        pb$tick(1)
        break
      }
    }
    if (is.null(discarded_variable))
      correlation_removed <- TRUE
  }

  if (change_rm) {
    pb$tick(total - removed - 2)
    if (!is.null(test))
      test <- model@test
    model <- trainMaxent(model@presence, model@background, old_rm, model@fc,
                         type = model@type, test = test, iter = model@iter)
    pb$tick(1)
  } else {
    pb$tick(total - removed)
  }

  removed_vars <- setdiff(vars, colnames(model@presence@data))
  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
