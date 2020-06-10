#' Variable Selection
#'
#' The function performs a data-driven variable selection. Starting from the
#' provided model it iterates through all the variables starting from the one
#' with the highest contribution (permutation importance or maxent percent
#' contribution). If the variable is correlated with other variables (according
#' to the given method and threshold) it performs a Jackknife test and among the
#' correlated variables it removes the one that results in the best performing
#' model when removed (according to the given metric for the training dataset).
#' The process is repeated until the remaining variables are not highly
#' correlated anymore.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param bg4cor \linkS4class{SWD} object. Background locations used to test the
#' correlation between environmental variables.
#' @param test \linkS4class{SWD}. Test dataset used to evaluate the model, not
#' used with aicc and \linkS4class{SDMmodelCV} objects, default is `NULL`.
#' @param env \link[raster]{stack} containing the environmental variables, used
#' only with "aicc", default is `NULL`.
#' @param parallel deprecated.
#' @param method character. The method used to compute the correlation matrix,
#' default "spearman".
#' @param cor_th numeric. The correlation threshold used to select highly
#' correlated variables, default is 0.7.
#' @param permut integer. Number of permutations, default is 10.
#' @param use_pc logical, use percent contribution. If `TRUE` and the model is
#' trained using the \linkS4class{Maxent} method, the algorithm uses the percent
#' contribution computed by Maxent software to score the variable importance,
#' default is `FALSE`.
#'
#' @details * To find highly correlated variables the following formula is used:
#' \deqn{| coeff | \le cor_th}
#'
#' @return The \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object trained
#' using the selected variables.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Prepare background locations to test autocorrelation, this usually gives a
#' # warning message given that less than 10000 points can be randomly sampled
#' bg_coords <- dismo::randomPoints(predictors, 10000)
#' bg <- prepareSWD(species = "Virtual species", a = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' \dontrun{
#' # Remove variables with correlation higher than 0.7 accounting for the AUC,
#' # in the following example the variable importance is computed as permutation
#' # importance
#' vs <- varSel(model, metric = "auc", bg4cor = bg, test = test, cor_th = 0.7,
#'              permut = 1)
#' vs
#'
#' # Remove variables with correlation higher than 0.7 accounting for the TSS,
#' # in the following example the variable importance is the MaxEnt percent
#' # contribution
#' # Train a model
#' # The next line checks if Maxent is correctly configured but you don't need
#' # to run it in your script
#' if (checkMaxentInstallation(verbose = FALSE)) {
#' model <- train(method = "Maxent", data = train, fc = "l")
#' vs <- varSel(model, metric = "tss", bg4cor = bg, test = test, cor_th = 0.7,
#'              use_pc = TRUE)
#' vs
#'
#' # Remove variables with correlation higher than 0.7 accounting for the aicc,
#' # in the following example the variable importance is the MaxEnt percent
#' # contribution
#' vs <- varSel(model, metric = "aicc", bg4cor = bg, cor_th = 0.7,
#'              use_pc = TRUE, env = predictors)
#' vs
#' }
#' }
#' }
varSel <- function(model, metric, bg4cor, test = NULL, env = NULL,
                   parallel = FALSE, method = "spearman", cor_th = 0.7,
                   permut = 10, use_pc = FALSE) {

  # TODO remove this code in a next release
  if (parallel)
    warning("parallel argument is deprecated and not used anymore",
            call. = FALSE, immediate. = TRUE)

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  .check_args(model, metric = metric, test = test, env = env)

  if (use_pc & .get_model_class(model) != "Maxent")
    stop(paste("Percent contribution cannot be used with model of method",
               .get_model_class(model)))

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  cor_vars <- corVar(bg4cor, method = method, cor_th = cor_th)
  cor_vars <- unique(c(cor_vars$Var1, cor_vars$Var2))

  total <- length(cor_vars)
  removed <- 0
  first_iter <- TRUE

  pb <- progress::progress_bar$new(
    format = "Var Selection [:bar] :percent in :elapsedfull", total = total,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  correlation_removed <- FALSE

  # Remove categorical environmental variables
  df <- bg4cor@data
  categorical <- names(Filter(is.factor, df))
  df[categorical] <- list(NULL)
  cor_matrix <- stats::cor(df, method = method)

  # metric used for chart
  train_metric <- data.frame(x = 0, y = .get_metric(metric, model, env = env))
  if (metric != "aicc") {
    val_metric <- data.frame(x = 0, y = .get_metric(metric, model, test = test))
  } else {
    val_metric <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Create chart
  initial_vars <- colnames(model@data@data)
  settings <- list(labels = initial_vars, metric = .get_metric_label(metric),
                   title = "Variable Selection", update = TRUE)

  line_title <- "Starting model"
  folder <- tempfile("SDMsel")

  while (correlation_removed == FALSE) {

    cor_matrix <- as.data.frame(cor_matrix)

    if (use_pc) {
      scores <- maxentVarImp(model)
    } else {
      scores <- suppressMessages(varImp(model, permut = permut))
    }

    vars <- scores$Variable
    discarded_variable <- NULL

    # Update chart
    index <- match(initial_vars, vars)
    vals <- scores[, 2][index]
    vals[is.na(vals)] <- 0
    data <- list(data = vals, train = train_metric, val = val_metric,
                 lineTitle = line_title, stop = FALSE)

    if (first_iter) {
      .create_chart(folder = folder, script = "varSelection.js",
                    settings = settings, data = data)
      .show_chart(folder, height = "maximize")
      first_iter <- FALSE
    } else {
      .update_data(folder, data = data)
    }

    for (i in seq_along(vars)) {

      if (vars[i] %in% categorical)
        next

      coeff <- cor_matrix[vars[i]]
      hcv <- row.names(coeff)[abs(coeff) >= cor_th]

      if (length(hcv) > 1) {
        jk_test <- suppressMessages(doJk(model, metric = metric, test = test,
                                         variables = hcv, with_only = FALSE,
                                         env = env, return_models = TRUE))

        # index for metric data frames
        x <- nrow(train_metric)

        if (metric != "aicc") {
          index <- which.max(jk_test$results[, 2])
          train_metric[x + 1, ] <- list(x = x, y = jk_test$results[index, 2])
          val_metric[x + 1, ] <- list(x = x, y = jk_test$results[index, 3])
        } else {
          index <- which.min(jk_test$results[, 2])
          train_metric[x + 1, ] <- list(x = x, y = jk_test$results[index, 2])
        }

        model <- jk_test$models_without[[index]]
        discarded_variable <- as.character(jk_test$results$Variable[index])
        cor_matrix[discarded_variable] <- NULL
        cor_matrix <- cor_matrix[!(row.names(cor_matrix) == discarded_variable), ]
        line_title <- c(line_title, paste("Removed", discarded_variable))
        removed <- removed + 1
        pb$tick(1)
        break
      }
    }
    if (is.null(discarded_variable)) {
      correlation_removed <- TRUE
      stop <- TRUE
      data <- list(data = vals, train = train_metric, val = val_metric,
                   lineTitle = line_title, stop = stop)
      .update_data(folder, data = data)
    }
  }

  pb$tick(total - removed)

  removed_vars <- setdiff(initial_vars, colnames(model@data@data))
  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
