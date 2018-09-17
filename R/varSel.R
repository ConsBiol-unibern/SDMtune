#' Variable Selection
#'
#' The function performs a data-driven variable selection. Starting from the
#' provided model it iterates through all the variables starting from the one
#' with the highest contribution (permutation importance). If the variable is
#' correlated with other variables (according to the given method and threshold)
#' it performs a Jackknife test and among the correlated variables it removes
#' the one that results in the best performing model when removed (according to
#' the given metric using the Train dataset). The process is repeated untill the
#' remaining variables are not highly correlated anymore.
#'
#' @param model SDMmodel object.
#' @param bg4cor SWD object. Background locations used to test the correlation
#' between environmental variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param reg integer. The value of the regularization paramiter to use during
#' computation, default is 0.001, see details.
#' @param method character. The method used to comput the correlation matrix,
#' default "spearman".
#' @param cor_th numeric. The correlation threshold used to select highly
#' correlated variables, default is 0.7.
#' @param permut integer. Number of permutations, default is 10.
#'
#' @details You need package \pkg{snow} to use parallel computation. Parallel
#' computation increases the speed only for big datasets due to the time
#' necessary to create the cluster. For **Maxnet** using a **reg** lower than
#' 0.1 the model won't converge!
#' We should write something more... I will refer to our paper for the explanations...
#'
#' @return The model with trained using the selected variables.
#' @export
#' @importFrom progress progress_bar
#' @importFrom stats cor
#'
#' @examples \dontrun{
#' varSel(model, bg, metric = "auc")}
#'
#' @author Sergio Vignali
varSel <- function(model, bg4cor, metric = c("auc", "tss", "aicc"), env = NULL,
                   parallel = FALSE, reg = 0.001, method = "spearman",
                   cor_th = 0.7, permut = 10) {

  if (class(bg4cor) != "SWD")
    stop("bg4cort must be a SWD object!")

  cor_vars <- corVar(bg4cor, method = method, cor_th = cor_th)
  cor_vars <- unique(c(as.character(cor_vars$Var1),
                       as.character(cor_vars$Var2)))
  total <- length(cor_vars)
  change_reg = FALSE

  if (reg != model@model@reg) {
    total <- total + 2
    change_reg = TRUE
  }
  removed <- 0

  pb <- progress::progress_bar$new(
    format = "Var Selection [:bar] :percent in :elapsedfull", total = total,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  metric <- match.arg(metric)
  correlation_removed <- FALSE
  initial_vars <- colnames(model@presence@data)
  model_method <- class(model@model)

  if (model_method == "Maxent") {
    iter <- model@model@iter
    extra_args <- model@model@extra_args
  } else {
    iter <- NULL
    extra_args <- NULL
  }

  if (change_reg) {
    old_reg <- model@model@reg
    model <- train(method = model_method, presence = model@presence,
                   bg = model@background, reg = reg, fc = model@model@fc,
                   iter = iter, extra_args = extra_args)
    pb$tick(1)
  }

  # Remove categorical environmental variables
  df <- bg4cor@data
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- cor(df, method = method)

  while (correlation_removed == FALSE) {

    cor_matrix <- as.data.frame(cor_matrix)
    scores <- varImp(model, permut = permut)
    vars <- scores$Variable
    discarded_variable <- NULL

    for (i in 1:length(vars)) {

      if (vars[i] %in% categorical)
        next

      coeff <- cor_matrix[vars[i]]
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

  if (change_reg) {
    pb$tick(total - removed - 2)
    model <- train(method = model_method, presence = model@presence,
                   bg = model@background, reg = old_reg, fc = model@model@fc,
                   iter = iter, extra_args = extra_args)
    pb$tick(1)
  } else {
    pb$tick(total - removed)
  }

  removed_vars <- setdiff(initial_vars, colnames(model@presence@data))
  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
