#' Variable Selection
#'
#' The function performs a data-driven variable selection. Starting from the
#' provided model it iterates through all the variables starting from the one
#' with the highest contribution (permutation importance or maxent percent
#' contribution). If the variable is correlated with other variables (according
#' to the given method and threshold) it performs a Jackknife test and among the
#' correlated variables it removes the one that results in the best performing
#' model when removed (according to the given metric). The process is repeated
#' untill the remaining variables are not highly correlated anymore.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \link{SWD}. Test dataset used to evaluate the model, not used
#' with aicc and \link{SDMmodelCV} objects, default is NULL.
#' @param bg4cor SWD object. Background locations used to test the correlation
#' between environmental variables.
#' @param env \link{stack} containing the environmental variables, used only
#' with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE. Used only with AICc.
#' @param reg integer. The value of the regularization paramiter to use during
#' computation, default is NULL, see details.
#' @param method character. The method used to comput the correlation matrix,
#' default "spearman".
#' @param cor_th numeric. The correlation threshold used to select highly
#' correlated variables, default is 0.7.
#' @param permut integer. Number of permutations, default is 10.
#' @param use_pc logical, use percent contribution. If TRUE and the model is
#' trained using the \link{Maxent} method, the algorithm uses the percent
#' contribution computed by Maxent software to score the varialble importance,
#' default is FALSE.
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
varSel <- function(model, metric, test = NULL, bg4cor, env = NULL,
                   parallel = FALSE, reg = NULL, method = "spearman",
                   cor_th = 0.7, permut = 10, use_pc = FALSE) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  if (use_pc & .get_model_class(model) != "Maxent")
    warning(paste("Percent contribution cannot be used with model of method",
                  .get_model_class(model)))

  if (class(model) == "SDMmodelCV")
    test <- TRUE

  cor_vars <- corVar(bg4cor, method = method, cor_th = cor_th)
  cor_vars <- unique(c(as.character(cor_vars$Var1),
                       as.character(cor_vars$Var2)))

  # Change reg if different from model reg
  if (!is.null(reg)) {
    old_reg <- .get_model_reg(model)
    change_reg <- reg != old_reg
  } else {
    change_reg <- FALSE
  }

  total <- length(cor_vars)
  if (change_reg)
    total <- total + 2

  removed <- 0

  pb <- progress::progress_bar$new(
    format = "Var Selection [:bar] :percent in :elapsedfull", total = total,
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  correlation_removed <- FALSE

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

  if (use_pc) {
    scores <- maxentVarImp(model)
  } else {
    scores <- suppressMessages(varImp(model, permut = permut))
  }

  vals <- scores[, 2]
  line_title <- "Starting model"
  line_footer <- paste("reg: ", .get_model_reg(model))
  draw_line_1 <- FALSE
  data = list(data = rep(0, length(initial_vars)), train = train_metric,
              val = val_metric, drawLine1 = draw_line_1, drawLine2 = FALSE,
              reg = reg, lineTitle = line_title, lineFooter = line_footer,
              stop = FALSE)
  folder <- tempfile("SDMsel")

  .create_chart(folder = folder, script = "varSelection.js",
                settings = settings, data = data, height = 600)
  Sys.sleep(1.5)

  if (change_reg) {
    model <- .create_model_from_settings(model, list("reg" = reg))

    if (use_pc) {
      scores <- maxentVarImp(model)
    } else {
      scores <- suppressMessages(varImp(model, permut = permut))
    }

    vals <- scores[, 2]
    # index for metric data frames
    x <- nrow(train_metric)
    train_metric[x + 1, ] <- list(x = x, y = .get_metric(metric, model,
                                                         env = env,
                                                         parallel = parallel))
    if (metric != "aicc")
      val_metric[x + 1, ] <- list(x = x, y = .get_metric(metric, model,
                                                         test = test))
    line_title <- c(line_title, "Change reg")
    line_footer <- c(line_footer, paste("reg: ", reg))
    draw_line_1 <- TRUE
    data = list(data = vals, train = train_metric, val = val_metric,
                drawLine1 = draw_line_1, drawLine2 = FALSE, reg = reg,
                lineTitle = line_title, lineFooter = line_footer, stop = FALSE)
    .update_chart(folder, data = data)
    pb$tick(1)
  }

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
    data = list(data = vals, train = train_metric, val = val_metric,
                drawLine1 = draw_line_1, drawLine2 = FALSE, reg = reg,
                lineTitle = line_title, lineFooter = line_footer, stop = FALSE)
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
        line_footer <- c(line_footer, paste("reg: ", reg))
        removed <- removed + 1
        pb$tick(1)
        break
      }
    }
    if (is.null(discarded_variable)) {
      correlation_removed <- TRUE
      stop <- ifelse(change_reg, FALSE, TRUE)
      data = list(data = vals, train = train_metric, val = val_metric,
                  drawLine1 = draw_line_1, drawLine2 = FALSE,
                  lineTitle = line_title, lineFooter = line_footer, stop = stop)
      .update_chart(folder, data = data)
      Sys.sleep(.1)
    }
  }

  if (change_reg) {
    pb$tick(total - removed - 2)
    model <- .create_model_from_settings(model, list("reg" = old_reg))

    if (use_pc) {
      scores <- maxentVarImp(model)
    } else {
      scores <- suppressMessages(varImp(model, permut = permut))
    }

    vars <- scores$Variable
    vals <- scores[, 2]
    # index for metric data frames
    x <- nrow(train_metric)
    train_metric[x + 1, ] <- list(x = x, y = .get_metric(metric, model,
                                                         env = env,
                                                         parallel = parallel))
    if (metric != "aicc")
      val_metric[x + 1, ] <- list(x = x, y = .get_metric(metric, model,
                                                         test = test))
    index <- match(initial_vars, vars)
    vals <- scores[, 2][index]
    vals[is.na(vals)] <- 0
    line_title <- c(line_title, "Change reg")
    line_footer <- c(line_footer, paste("reg: ", reg))
    data = list(data = vals, train = train_metric, val = val_metric,
                drawLine1 = draw_line_1, drawLine2 = TRUE, reg = c(reg, old_reg),
                lineTitle = line_title, lineFooter = line_footer, stop = TRUE)
    .update_chart(folder, data = data)
    pb$tick(1)
  } else {
    pb$tick(total - removed)
  }

  removed_vars <- setdiff(initial_vars, colnames(model@p@data))
  message(paste("Removed variables:", paste(removed_vars, collapse = ", ")))

  return(model)
}
