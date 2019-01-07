#' Optimise Model
#'
#' @param model SDMmodel, SDMmodelCV object or a list containing models, see
#' details.
#' @param bg4test SWD object. Background locations used to get subsamples.
#' @param regs numeric vector with the regularization values to be tested.
#' @param fcs character vector with the feature combunation values to be tested.
#' @param bgs numeric vector with the number of background location to be
#' tested.
#' @param test SWD. Test dataset used to evaluate the model, not used with aicc
#' and SDMmodelCV objects, default is NULL.
#' @param pop numeric. Size of the population.
#' @param gen numeric. Number of generations.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc", default is "auc".
#' @param env \link{stack} or \link{brick} containing the environmental
#' variables, used only with "aicc", default is NULL.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is
#' FALSE.
#' @param keep_best numeric. Percentage of the best models in the population to
#' be retained during each iteration, expressed as decimal number. Default
#' is 0.4.
#' @param keep_random numeric. Probability of retaining the excluded models
#' during each iteration, expressed as decimal number. Default is 0.1.
#' @param mutation_chance numeric. Probability of mutation of the child models,
#' expressed as decimal number. Default is 0.4.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is NULL.
#'
#' @details You can continue to breed the same population of models passing the
#' output of the function (i.e. a list of models) as fist argument. See vignette
#' for a full explanation of the algorithm.
#'
#' @return list containing the models of the last population.
#' @export
#' @importFrom progress progress_bar
#' @importFrom stats runif
#'
#' @examples \dontrun{output <- optimiseModel(my_model, bg, regs = c(0.5, 1,
#' 1.5), fcs = c("lq", "lqp", "lqph"), bgs = c(5000, 10000, 15000),
#' test = my_val, pop = 20, gen = 10, seed = 25)}
#'
#' @author Sergio Vignali
optimiseModel <- function(model, bg4test, regs, fcs, bgs, test, pop, gen,
                          metric = c("auc", "tss", "aicc"), env = NULL,
                          parallel = FALSE, keep_best = 0.4, keep_random = 0.2,
                          mutation_chance = 0.4, seed = NULL) {

  metric <- match.arg(metric)

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) %in% c("SDMmodel", "list")) {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    test <- TRUE
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  kept_good <- round(pop * keep_best)
  kept_bad <- round(pop * keep_random)
  kept <- kept_good + kept_bad
  remaining <- pop - kept
  tot_models <- get_total_models(pop, gen, remaining)
  pb <- progress::progress_bar$new(
    format = "Optimise Model [:bar] :percent in :elapsedfull",
    total = (tot_models + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

  vars <- colnames(model@presence@data)
  bg4test@data <- bg4test@data[vars]
  bg_folds <- sample(nrow(bg4test@data))

  best_train <- rep(NA_real_, gen + 2)
  best_val <- rep(NA_real_, gen + 2)
  best_train[1] <- get_metric(metric, model, env = env, parallel = parallel)
  if (metric != "aicc") {
    best_val[1] <- get_metric(metric, model, test = test)
  } else {
    best_val = NULL
  }
  line_title = "Starting model"
  labels <- jsonlite::toJSON(c("start", as.character(0:gen)))

  context = list(pop = pop,
                 gen = gen,
                 tot_models = tot_models,
                 metric = get_metric_label(metric),
                 labels = labels)

  folder <- create_chart(template = "optimiseTemplate", context = context,
                         height = 500)

  update_chart(folder, data = list(best_train = best_train, best_val = best_val,
                                   lineTitle = line_title, n = 0))

  # Create random population
  models <- vector("list", length = pop)
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)
  scatter_footer <- vector("character", length = pop)

  for (i in 1:pop) {
    models[[i]] <- create_random_model(model, bg4test = bg4test,
                                       bg_folds = bg_folds, regs = regs,
                                       fcs = fcs, bgs = bgs)
    train_metric[i, ] <- list(i, get_metric(metric, models[[i]], env = env,
                                            parallel = parallel))
    if (metric != "aicc")
      val_metric[i, ] <- list(i, get_metric(metric, models[[i]], test))
    scatter_footer[i] <- get_model_hyperparams(models[[i]])
    update_chart(folder, data = list(train = train_metric, val = val_metric,
                                     n = i, gen = 0,
                                     scatterFooter = scatter_footer,
                                     best_train = best_train,
                                     best_val = best_val,
                                     lineTitle = line_title))
    Sys.sleep(0.2)
    pb$tick(1)
  }

  metrics <- list(train_metric$y, val_metric$y)
  rank_index <- get_rank_index(metric, metrics)

  if (!is.logical(rank_index)) {
    models <- models[rank_index]
    train_metric <- data.frame(x = seq(1, pop), y = metrics[[1]][rank_index])
    val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])
    scatter_footer <- scatter_footer[rank_index]
    best_train[2] <- train_metric[1, 2]
    if (metric != "aicc")
      best_val[2] <- val_metric[1, 2]
    line_title <- c(line_title, "Generation 0")
    update_chart(folder, data = list(train = train_metric, val = val_metric,
                                     n = pop, gen = 0,
                                     scatterFooter = scatter_footer,
                                     best_train = best_train,
                                     best_val = best_val,
                                     lineTitle = line_title))
    Sys.sleep(0.2)
  } else {
    stop("The models in the random population are all overfitting the validation dataset!")
  }

  for (i in 1:gen) {
    index_kept <- c(1:kept_good, sample((kept + 1):pop,
                                        kept_bad))
    train_metric <- train_metric[index_kept, ]
    train_metric$x <- 1:kept
    if (metric != "aicc") {
      val_metric <- val_metric[index_kept, ]
      val_metric$x <- 1:kept
    }
    scatter_footer <- scatter_footer[index_kept]

    n <- pop + (remaining * (i - 1))
    update_chart(folder, data = list(train = train_metric, val = val_metric,
                                     n = n, gen = i,
                                     scatterFooter = scatter_footer,
                                     best_train = best_train,
                                     best_val = best_val,
                                     lineTitle = line_title))
    Sys.sleep(0.2)
    parents <- models[index_kept]
    new_models <- parents

    for (j in 1:remaining) {

      couple <- sample(parents, size = 2)
      mother <- couple[[1]]
      father <- couple[[2]]
      child <- breed(mother, father, bg4test, bg_folds, regs, fcs, bgs,
                     mutation_chance)
      train_metric[kept + j, ] <- list(kept + j, get_metric(metric, child,
                                                            env = env,
                                                            parallel = parallel)
                                       )
      if (metric != "aicc")
        val_metric[kept + j, ] <- list(kept + j, get_metric(metric, child,
                                                            test))
      scatter_footer[kept + j] <- get_model_hyperparams(child)

      new_models <- c(new_models, child)
      n <- pop + (remaining * (i - 1)) + j
      update_chart(folder, data = list(train = train_metric, val = val_metric,
                                       n = n, gen = i,
                                       scatterFooter = scatter_footer,
                                       best_train = best_train,
                                       best_val = best_val,
                                       lineTitle = line_title))
      Sys.sleep(0.2)
      pb$tick(1)
    }
    metrics <- list(train_metric$y, val_metric$y)
    rank_index <- get_rank_index(metric, metrics)

    if (!is.logical(rank_index)) {
      models <- new_models[rank_index]
      train_metric <- data.frame(x = seq(1, pop), y = metrics[[1]][rank_index])
      val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])
      scatter_footer <- scatter_footer[rank_index]
      n <- tot_models + 1
      best_train[i + 2] <- train_metric[1, 2]
      if (metric != "aicc")
        best_val[i + 2] <- val_metric[1, 2]
      line_title <- c(line_title, paste("Generation", i))
      update_chart(folder, data = list(train = train_metric, val = val_metric,
                                       n = n, gen = i,
                                       scatterFooter = scatter_footer,
                                       best_train = best_train,
                                       best_val = best_val,
                                       lineTitle = line_title))
      Sys.sleep(0.2)
    } else {
      stop(paste("Optimization algorithm interrupted at population", i,
                 "because it starts to overfit validation dataset!"))
    }
  }
  metrics <- list(train_metric$y, val_metric$y)
  output <- create_optimise_output(models, metric, metrics)
  pb$tick(1)
  return(output)
}

create_random_model <- function(model, bg4test, bg_folds, regs, fcs, bgs) {

  if (class(model) == "SDMmodel") {
    rep <- 1
    method <- class(model@model)
    folds <- NULL
    object <- model
  } else {
    rep <- length(model@models)
    method <- class(model@models[[1]]@model)
    folds <- model@folds
    object <- model@models[[1]]
  }

  reg <- sample(regs, size = 1)
  fc <- sample(fcs, size = 1)
  n_bg <- sample(bgs, size = 1)

  bg <- bg4test
  bg@data <- bg4test@data[bg_folds[1:n_bg], ]
  bg@coords <- bg4test@coords[bg_folds[1:n_bg], ]

  if (method == "Maxent") {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       folds = folds, iter = object@model@iter,
                       extra_args = object@model@extra_args)
  } else {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       folds = folds)
  }

  return(new_model)
}

breed <- function(mother, father, bg4test, bg_folds, regs, fcs, bgs,
                  mutation_chance) {

  if (class(mother) == "SDMmodel") {
    rep <- 1
    method <- class(mother@model)
    reg <- sample(c(mother@model@reg, father@model@reg), size = 1)
    fc <- sample(c(mother@model@fc, father@model@fc), size = 1)
    bg <- sample(c(mother@background, father@background), size = 1)[[1]]
    object <- mother
  } else {
    rep <- length(mother@models)
    method <- class(mother@models[[1]]@model)
    reg <- sample(c(mother@models[[1]]@model@reg,
                    father@models[[1]]@model@reg), size = 1)
    fc <- sample(c(mother@models[[1]]@model@fc,
                   father@models[[1]]@model@fc), size = 1)
    bg <- sample(c(mother@models[[1]]@background,
                   father@models[[1]]@background), size = 1)[[1]]
    object <- mother@models[[1]]
  }

  if (mutation_chance > runif(1)) {
    mutation <- sample(c("reg", "fc", "bg"), size = 1)
    if (mutation == "reg") {
      parents_regs <- c(get_model_reg(mother), get_model_reg(father))
      regs <- setdiff(regs, parents_regs)
      reg <- sample(regs, size = 1)
    } else if (mutation == "fc") {
      parents_fcs <- c(get_model_fc(mother), get_model_fc(father))
      fcs <- setdiff(fcs, parents_fcs)
      fc <- sample(fcs, size = 1)
    } else {
      parents_bgs <- c(nrow(mother@background@data),
                       nrow(father@background@data))
      bgs <- setdiff(bgs, parents_bgs)
      n_bg <- sample(bgs, size = 1)
      bg@data <- bg4test@data[bg_folds[1:n_bg], ]
      bg@coords <- bg4test@coords[bg_folds[1:n_bg], ]
    }
  }

  if (method == "Maxent") {
    new_model <- train(method = method, presence = object@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       iter = object@model@iter,
                       extra_args = object@model@extra_args)
  } else {
    new_model <- train(method = method, presence = object@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE)
  }

  return(new_model)
}

mutate <- function(model, mother, father, bg4test, bg_folds, regs, fcs, bgs) {

  mutation <- sample(c("reg", "fc", "bg"), size = 1)

  if (class(model) == "SDMmodel") {
    rep <- 1
    method <- class(model@model)
    folds <- NULL
    object <- model
    reg <- model@model@reg
    fc <- model@model@fc
  } else {
    rep <- length(model@models)
    method <- class(model@models[[1]]@model)
    folds <- model@folds
    object <- model@models[[1]]
    reg <- model@models[[1]]@model@reg
    fc <- model@models[[1]]@model@fc
  }
  bg <- model@background

  if (mutation == "reg") {
    if (length(regs) > 2) {
      parents_regs <- c(get_model_reg(mother), get_model_reg(father))
      regs <- setdiff(regs, parents_regs)
    } else {
      regs <- setdiff(regs, reg)
    }
    reg <- sample(regs, size = 1)
  } else if (mutation == "fc") {
    if (length(fcs) > 2) {
      parents_fcs <- c(get_model_fc(mother), get_model_fc(father))
      fcs <- setdiff(fcs, parents_fcs)
    } else {
      fcs <- setdiff(fcs, fc)
    }
    fc <- sample(fcs, size = 1)
  } else {
    if (length(bgs) > 2) {
      parents_bgs <- c(nrow(mother@background@data),
                       nrow(father@background@data))
      bgs <- setdiff(bgs, parents_bgs)
    } else {
      bgs <- setdiff(bgs, nrow(model@background@data))
    }
    n_bg <- sample(bgs, size = 1)
    bg@data <- bg4test@data[bg_folds[1:n_bg], ]
    bg@coords <- bg4test@coords[bg_folds[1:n_bg], ]
  }

  if (method == "Maxent") {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       folds = folds, iter = object@model@iter,
                       extra_args = object@model@extra_args)
  } else {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       folds = folds)
  }

  return(new_model)
}
