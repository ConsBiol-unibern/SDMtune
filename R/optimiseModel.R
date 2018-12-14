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
#' @importFrom stats rnorm
#'
#' @examples \dontrun{output <- optimiseModel(my_model, bg, regs = c(0.5, 1,
#' 1.5), fcs = c("lq", "lqp", "lqph"), bgs = c(5000, 10000, 15000),
#' test = my_val, pop = 20, gen = 10, seed = 25)}
#'
#' @author Sergio Vignali
optimiseModel <- function(model, bg4test, regs, fcs, bgs, test, pop, gen,
                          metric = c("auc", "tss", "aicc"), env = NULL,
                          parallel = FALSE, keep_best = 0.4, keep_random = 0.1,
                          mutation_chance = 0.4, seed = NULL) {

  metric <- match.arg(metric)

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel") {
    if (is.null(test) & metric != "aicc")
      stop("You need to provide a test dataset!")
  } else {
    test <- TRUE
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }

  pb <- progress::progress_bar$new(
    format = "Optimise Model [:bar] :percent in :elapsedfull",
    total = (gen + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

  if (typeof(model) != "list") {
    vars <- colnames(model@presence@data)
    bg4test@data <- bg4test@data[vars]
    bg_folds <- sample(nrow(bg4test@data))
    models <- create_population(model, size = pop, bg4test = bg4test,
                                bg_folds = bg_folds, regs = regs, fcs = fcs,
                                bgs = bgs)
  } else {
    vars <- colnames(model[[1]]@presence@data)
    bg4test@data <- bg4test@data[vars]
    bg_folds <- sample(nrow(bg4test@data))
    models <- model
  }

  rank <- rank_models(models, test, metric = metric, env = env,
                      parallel = parallel)

  if (!is.logical(rank)) {
    models <- rank
    pb$message(paste("Training AUC:", auc(models[[1]]), "Testing AUC:", auc(models[[1]], test)))
  } else {
    stop("The models in the random population are all overfitting the validation dataset!")
  }
  pb$tick(1)

  for (i in 1:gen) {
    kept <- round((pop * keep_best), 0)
    parents <- models[1:kept]

    for (mod in models[(kept + 1):pop]) {
      if (keep_random > rnorm(1))
        parents <- c(parents, mod)
    }

    remaining <- pop - length(parents)
    new_models <- parents

    for (j in 1:remaining) {
      couple <- sample(parents, size = 2)
      mother <- couple[[1]]
      father <- couple[[2]]
      child <- breed(mother, father)

      if (mutation_chance > rnorm(1))
        child <- mutate(child, bg4test = bg4test, bg_folds = bg_folds,
                        regs = regs, fcs = fcs, bgs = bgs)

      new_models <- c(new_models, child)
    }
    rank <- rank_models(models, test, metric = metric, env = env,
                        parallel = parallel)
    if (!is.logical(rank)) {
      models <- new_models
      pb$message(paste("Training AUC:", auc(models[[1]]), "Testing AUC:", auc(models[[1]], test)))
      pb$tick(1)
    } else {
      message(paste("Optimization algorithm interrupted at population", i,
                    "because it starts to overfit validation dataset!"))
      pb$tick(gen + 1 - i)
    }

  }
  gc()

  return(models)
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

  gc()

  return(new_model)
}

create_population <- function(model, size, bg4test, bg_folds, regs, fcs, bgs) {
  models <- vector("list", length = size)

  for (i in 1:size) {
    models[[i]] <- create_random_model(model, bg4test = bg4test,
                                       bg_folds = bg_folds, regs = regs,
                                       fcs = fcs, bgs = bgs)
  }
  return(models)
}

rank_models <- function(models, test, metric, env, parallel) {

  values <- c()
  good_models <- c()
  bad_models <- c()

  for (i in 1:length((models))) {

    if (metric == "auc") {
      train_metric <- auc(models[[i]])
      test_metric <- auc(models[[i]], test)
      diff_metric <- train_metric - test_metric
    } else if (metric == "tss") {
      train_metric <- tss(models[[i]])
      test_metric <- tss(models[[i]], test)
      diff_metric <- train_metric - test_metric
    } else {
      values <- c(values, aicc(models[[i]], env, parallel))
    }

    if (metric != "aicc") {
      if (diff_metric >= 0) {
        good_models <- c(good_models, models[[i]])
        values <- c(values, test_metric)
      } else {
        bad_models <- c(bad_models, models[[i]])
      }
    } else {
      good_models <- c(good_models, models[[i]])
    }
  }

  if (length(values) != 0) {
    index <- order(-values)
    models <- c(good_models[index], bad_models)
    overfit <- FALSE
  } else {
    models <- bad_models
    overfit <- TRUE
  }

  gc()

  if (overfit) {
    return(FALSE)
  } else {
    return(models)
  }
}

breed <- function(mother, father) {

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

  if (method == "Maxent") {
    new_model <- train(method = method, presence = object@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE,
                       iter = object@model@iter,
                       extra_args = object@model@extra_args)
  } else {
    new_model <- train(method = method, presence = object@presence, bg = bg,
                       reg = reg, fc = fc, replicates = rep, verbose = FALSE)
  }

  gc()

  return(new_model)
}

mutate <- function(model, bg4test, bg_folds, regs, fcs, bgs) {

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
    regs <- regs[regs != reg]
    reg <- sample(regs, size = 1)
  } else if (mutation == "fc") {
    fcs <- fcs[fcs != fc]
    fc <- sample(fcs, size = 1)
  } else {
    bgs <- bgs[bgs != nrow(model@background@data)]
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

  gc()

  return(new_model)
}
