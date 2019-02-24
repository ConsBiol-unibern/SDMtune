#' Variable Selection
#'
#' The function performs a data-driven variable selection. Starting from the
#' provided model it iterates through all the variables starting from the one
#' with the highest contribution (permutation importance). If the variable is
#' correlated with other variables (according to the given method and threshold)
#' it performs a Jackknife test and among the correlated variables it removes
#' the one that results in the best performing model when removed (according to
#' the given metric using the test dataset). The process is repeated untill the
#' remaining variables are not highly correlated anymore.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param bg4cor SWD object. Background locations used to test the correlation
#' between environmental variables.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param test \link{SWD}. Test dataset used to evaluate the model, not used
#' with aicc and \link{SDMmodelCV} objects, default is NULL.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param reg integer. The value of the regularization paramiter to use during
#' computation, default is 0.1, see details.
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
#' We should write something more... I will refer to our paper for the
#' explanations...
#'
#' @return The \link{SDMmodel} or \link{SDMmodelCV} object trained using the
#' selected variables.
#' @export
#' @importFrom progress progress_bar
#' @importFrom stats cor
#'
#' @examples \dontrun{
#' varSel(model, bg, metric = "auc")}
#'
#' @author Sergio Vignali
varSel <- function(model, bg4cor, metric = c("auc", "tss", "aicc"), test = NULL,
                   env = NULL, parallel = FALSE, reg = 0.1, method = "spearman",
                   cor_th = 0.7, permut = 10) {

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  if (class(model) == "SDMmodel") {
    rep <- 1
    model_method <- class(model@model)
    folds <- NULL
    object <- model
  } else {
    rep <- length(model@models)
    model_method <- class(model@models[[1]]@model)
    folds <- model@folds
    object <- model@models[[1]]
    test <- TRUE
  }

  cor_vars <- corVar(bg4cor, method = method, cor_th = cor_th)
  cor_vars <- unique(c(as.character(cor_vars$Var1),
                       as.character(cor_vars$Var2)))
  change_reg <- reg != object@model@reg
  total <- length(cor_vars)
  if (change_reg)
    total <- total + 2

  removed <- 0

  pb <- progress::progress_bar$new(
    format = "Var Selection [:bar] :percent in :elapsedfull", total = total,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  metric <- match.arg(metric)
  correlation_removed <- FALSE

  if (change_reg) {
    if (method == "Maxent") {
      model <- train(method = model_method, p = model@p,
                     a = model@a, reg = reg, fc = object@model@fc,
                     replicates = rep, verbose = FALSE, folds = folds,
                     iter = object@model@iter,
                     extra_args = object@model@extra_args)
    } else {
      model <- train(method = model_method, p = model@p,
                     a = model@a, reg = reg, fc = object@model@fc,
                     replicates = rep, verbose = FALSE, folds = folds)
    }
    pb$tick(1)
  }

  # Remove categorical environmental variables
  df <- bg4cor@data
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- cor(df, method = method)

  # metric used for chart
  train_metric <- data.frame(x = 0, y = .get_metric(metric, model, env = env,
                                                    parallel = parallel))
  if (metric != "aicc") {
    val_metric <- data.frame(x = 0, y = .get_metric(metric, model, test = test))
  } else {
    val_metric <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Create chart
  initial_vars <- colnames(model@p@data)
  settings <- list(labels = initial_vars, metric = .get_metric_label(metric),
                   update = TRUE)
  data = list(data = rep(0, length(initial_vars)), stop = FALSE)
  folder <- tempfile("SDMsel")

  .create_chart(folder = folder, script = "varSelection.js",
                settings = settings, data = data, height = 600)
  Sys.sleep(1.5)

  while (correlation_removed == FALSE) {

    cor_matrix <- as.data.frame(cor_matrix)
    scores <- suppressMessages(varImp(model, permut = permut))
    vars <- scores$Variable
    discarded_variable <- NULL

    # Update chart
    index <- match(initial_vars, vars)
    vals <- scores[, 2][index]
    vals[is.na(vals)] <- 0
    data = list(data = vals, train = train_metric, val = val_metric,
                stop = FALSE)
    .update_chart(folder, data = data)
    Sys.sleep(.1)

    for (i in 1:length(vars)) {

      if (vars[i] %in% categorical)
        next

      coeff <- cor_matrix[vars[i]]
      hcv <- row.names(coeff)[abs(coeff) >= cor_th]

      if (length(hcv) > 1) {
        jk_test <- suppressMessages(doJk(model, metric = metric, test = test,
                                         variables = hcv, with_only = FALSE,
                                         env = env, parallel = parallel,
                                         return_models = TRUE))

        # index for metric data frames
        x <- nrow(train_metric) + 1

        if (metric != "aicc") {
          index <- which.max(jk_test$results[, 2])
          train_metric[x, ] <- list(x = x - 1, y = jk_test$results[index, 2])
          val_metric[x, ] <- list(x = x - 1, y = jk_test$results[index, 3])
        } else {
          index <- which.min(jk_test$results[, 2])
          train_metric[x, ] <- list(x = x - 1, y = jk_test$results[index, 2])
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
    if (is.null(discarded_variable)) {
      correlation_removed <- TRUE
      .update_chart(folder, data = list(data = vals, train = train_metric,
                                        val = val_metric, stop = TRUE))
      Sys.sleep(.1)
    }
  }

  if (change_reg) {
    pb$tick(total - removed - 2)
    if (method == "Maxent") {
      model <- train(method = model_method, p = model@p,
                     a = object@a, reg = object@model@reg,
                     fc = object@model@fc, replicates = rep, verbose = FALSE,
                     folds = folds, iter = object@model@iter,
                     extra_args = object@model@extra_args)
    } else {
      model <- train(method = model_method, p = model@p,
                     a = model@a, reg = object@model@reg,
                     fc = object@model@fc, replicates = rep, verbose = FALSE,
                     folds = folds)
    }
    pb$tick(1)
  } else {
    pb$tick(total - removed)
  }

  removed_vars <- setdiff(initial_vars, colnames(model@p@data))
  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
