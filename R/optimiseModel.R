#' Optimise Model
#'
#' @param model SDMmodel object.
#' @param bg SWD object. Background locations used to get subsamples.
#' @param regs numeric vector with the regularization values to be tested.
#' @param fcs character vector with the feature combunation values to be tested.
#' @param n_bgs numeric vector with the number of background location to be
#' tested.
#' @param gen numeric. Number of generations.
#' @param pop numeric. Size of the population.
#' @param test SWD object containing the validation dataset.
#' @param keep numeric. Percentage of the best models in the population to be
#' retained during each iteration, expressed as decimal number. Default is 0.4.
#' @param random_keep numeric. Probability of retaining the excluded models
#' during each iteration, expressed as decimal number. Default is 0.1.
#' @param mutation_chance numeric. Probability of mutation of the child models,
#' expressed as decimal number. Default is 0.2.
#' @param seed numeric. The value used to set the seed to have consistent
#' results, default is NULL.
#'
#' @details See vignette for a full explanation of the algorithm.
#'
#' @return list containing the models of the last population.
#' @export
#'
#' @examples \dontrun{output <- optimiseModel(my_model, bg, regs = c(0.5, 1,
#' 1.5), fcs = c("lq", "lqp", "lqph"), n_bgs = c(5000, 10000, 15000), gen = 5,
#' pop = 20, val = my_val, seed = 25)}
#'
#' @author Sergio Vignali
optimiseModel <- function(model, bg, regs, fcs, n_bgs, gen, pop, val,
                          keep = 0.4, random_keep = 0.1,
                          mutation_chance = 0.2, seed = NULL) {

  if (!is.null(seed))
    set.seed(seed)

  vars <- colnames(model@presence@data)
  bg@data <- bg@data[vars]
  parents <- c()

  models <- create_population(model, size = pop, bg = bg, regs = regs,
                              fcs = fcs, n_bgs = n_bgs)

  for (i in 1:gen) {
    models <- rank_models(models, val)
    kept <- round((pop * keep), 0)
    parents <- models[1:kept]

    for (mod in models[(kept + 1):pop]) {
      if (random_keep > rnorm(1))
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
        child <- mutate(child, bg = bg, regs = regs, fcs = fcs, n_bgs = n_bgs)

      new_models <- c(new_models, child)
    }
    models <- new_models
  }
  models <- rank_models(models, val)
  gc()

  return(models)
}

create_random_model <- function(model, bg, regs, fcs, n_bgs) {

  method <- class(model@model)
  reg <- sample(regs, size = 1)
  fc <- sample(fcs, size = 1)
  n_bg <- sample(n_bgs, size = 1)

  folds <- sample(nrow(bg@data))
  bg@data <- bg@data[folds[1:n_bg], ]

  if (method == "Maxent") {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, iter = model@model@iter,
                       extra_args = model@model@extra_args)
  } else {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc)
  }

  gc()

  return(new_model)
}

create_population <- function(model, size, bg, regs, fcs, n_bgs) {
  models <- vector("list", length = size)

  for (i in 1:size) {
    models[[i]] <- create_random_model(model, bg = bg, regs = regs, fcs = fcs,
                                       n_bgs = n_bgs)
  }
  return(models)
}

rank_models <- function(models, test) {

  test_aucs <- c()
  good_models <- bad_models <- c()

  for (i in 1:length((models))) {
    train_auc <- auc(models[[i]])
    test_auc <- auc(models[[i]], test)
    diff_auc <- train_auc - test_auc

    if (diff_auc >= 0) {
      good_models <- c(good_models, models[[i]])
      test_aucs <- c(test_aucs, test_auc)
    } else {
      bad_models <- c(bad_models, models[[i]])
    }
  }

  index <- order(-test_aucs)
  models <- c(good_models[index], bad_models)
  gc()

  return(models)
}

breed <- function(mother, father) {

  method <- class(mother@model)
  reg <- sample(c(mother@model@reg, father@model@reg), size = 1)
  fc <- sample(c(mother@model@fc, father@model@fc), size = 1)
  bg <- sample(c(mother@background, father@background), size = 1)[[1]]

  if (method == "Maxent") {
    new_model <- train(method = method, presence = mother@presence, bg = bg,
                       reg = reg, fc = fc, iter = model@model@iter,
                       extra_args = model@model@extra_args)
  } else {
    new_model <- train(method = method, presence = mother@presence, bg = bg,
                       reg = reg, fc = fc)
  }

  gc()

  return(new_model)
}

mutate <- function(model, bg, regs, fcs, n_bgs) {

  mutation <- sample(c("reg", "fc", "bg"), size = 1)
  method <- class(model@model)

  if (mutation == "reg") {
    reg <- sample(regs, size = 1)
    fc <- model@model@fc
    bg <- model@background
  } else if (mutation == "fc") {
    reg <- model@model@reg
    fc <- sample(fcs, size = 1)
    bg <- model@background
  } else {
    reg <- model@model@reg
    fc <- model@model@fc
    n_bg <- sample(n_bgs, size = 1)
    folds <- sample(nrow(bg@data))
    bg@data <- bg@data[folds[1:n_bg], ]
  }

  if (method == "Maxent") {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc, iter = model@model@iter,
                       extra_args = model@model@extra_args)
  } else {
    new_model <- train(method = method, presence = model@presence, bg = bg,
                       reg = reg, fc = fc)
  }

  gc()

  return(new_model)
}
