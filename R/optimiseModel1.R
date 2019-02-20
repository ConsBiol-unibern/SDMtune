#' @export
optimiseModel1 <- function(model, hypers, bg4test = NULL, test = NULL,
                           pop = 20, gen = 5, metric = c("auc", "tss", "aicc"),
                           env = NULL, parallel = FALSE, keep_best = 0.4,
                           keep_random = 0.2, mutation_chance = 0.4,
                           seed = NULL) {

  metric <- match.arg(metric)

  if (metric == "aicc" & is.null(env) & class(model) == "SDMmodel")
    stop("You must provide env argument if you want to use AICc metric!")

  if (class(model) == "SDMmodel" & is.null(test) & metric != "aicc") {
    stop("You need to provide a test dataset!")
  }
  if (class(model) == "SDMmodelCV") {
    test <- TRUE
    if (metric == "aicc")
      stop("Metric aicc not allowed with SDMmodelCV objects!")
  }
  if (!is.null(hypers$bg) & is.null(bg4test))
    stop("bg4test must be provided to tune background locations!")

  if (sum(lengths(hypers) > 2) < 1)
    stop("One hyperparameter in hyoers should have more than two values to allow crossover!")

  if (keep_best + keep_random > 1)
    stop("Sum of 'keep_best' and 'keep_random' cannot be more than 1!")

  # Check if at least two hyperparameters have more than one value
  # TODO.check_hyperparams_validity(bgs, fcs, regs)

  kept_good <- round(pop * keep_best)
  kept_bad <- round(pop * keep_random)
  kept <- kept_good + kept_bad
  remaining <- pop - kept
  tot_models <- .get_total_models(pop, gen, remaining)
  pb <- progress::progress_bar$new(
    format = "Optimise Model [:bar] :percent in :elapsedfull",
    total = (tot_models + 1), clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  if (!is.null(seed))
    set.seed(seed)

  models <- vector("list", length = pop)
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)
  scatter_footer <- vector("character", length = pop)

  if (!is.null(hypers$bg)) {
    vars <- colnames(model@presence@data)
    bg4test@data <- bg4test@data[vars]
    bg_folds <- sample(nrow(bg4test@data))
  } else {
    bg_folds <- NULL
  }

  best_train <- rep(NA_real_, gen + 2)
  best_val <- rep(NA_real_, gen + 2)
  best_train[1] <- .get_metric(metric, model, env = env, parallel = parallel)
  if (metric != "aicc") {
    best_val[1] <- .get_metric(metric, model, test = test)
  } else {
    best_val <- NULL
  }
  line_title <- "Starting model"
  line_footer <- .get_model_hyperparams(model)

  settings <- list(pop = pop,
                   gen = gen,
                   metric = .get_metric_label(metric),
                   labels = c("start", 0:gen),
                   update = TRUE)

  data = list(gen = 0,
              best_train = best_train,
              best_val = best_val,
              lineTitle = line_title,
              lineFooter = line_footer,
              stop = FALSE)

  folder <- tempfile("SDMtune")

  .create_chart(folder = folder, script = "optimiseModel.js",
                settings = settings, data = data, height = 500)

  # Create data frame with all possible combinations of hyperparameters
  tunable_args <- .get_train_args(model)[.get_tunable_args(model)]
  tunable_args[names(hypers)] <- hypers
  if (is.null(hypers$bg))
    tunable_args$bg <- nrow(model@background@data)
  settings <- expand.grid(tunable_args, stringsAsFactors = FALSE)
  # Get the index of n = pop random configurations
  index <- sample(nrow(settings))

  # Create random population
  for (i in 1:pop) {
    models[[i]] <- .create_random_model(model, settings[index[i], ],
                                        bg4test = bg4test, bg_folds = bg_folds)
    train_metric[i, ] <- list(i, .get_metric(metric, models[[i]], env = env,
                                             parallel = parallel))
    if (metric != "aicc")
      val_metric[i, ] <- list(i, .get_metric(metric, models[[i]], test))
    scatter_footer[i] <- .get_model_hyperparams(models[[i]])
    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      gen = 0, scatterFooter = scatter_footer,
                                      best_train = best_train,
                                      best_val = best_val,
                                      lineTitle = line_title,
                                      lineFooter = line_footer, stop = FALSE))
    Sys.sleep(.1)
    pb$tick(1)
  }

  metrics <- list(train_metric$y, val_metric$y)
  rank_index <- .get_rank_index(metric, metrics)

  if (!is.logical(rank_index)) {
    models <- models[rank_index]
    train_metric <- data.frame(x = seq(1, pop), y = metrics[[1]][rank_index])
    val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])
    scatter_footer <- scatter_footer[rank_index]
    best_train[2] <- train_metric[1, 2]
    if (metric != "aicc")
      best_val[2] <- val_metric[1, 2]
    line_title <- c(line_title, "Generation 0")
    line_footer <- c(line_footer, .get_model_hyperparams(models[[1]]))
    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      gen = 0, scatterFooter = scatter_footer,
                                      best_train = best_train,
                                      best_val = best_val,
                                      lineTitle = line_title,
                                      lineFooter = line_footer, stop = FALSE))
    Sys.sleep(.1)
  } else {
    stop(paste("Optimization algorithm interrupted at generation", 0,
               "because it overfits validation dataset!"))
  }

  for (i in 1:gen) {
    index_kept <- c(1:kept_good, sample( (kept_good + 1):pop, kept_bad))
    train_metric <- train_metric[index_kept, ]
    train_metric$x <- 1:kept
    if (metric != "aicc") {
      val_metric <- val_metric[index_kept, ]
      val_metric$x <- 1:kept
    }
    scatter_footer <- scatter_footer[index_kept]

    .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                      gen = i, scatterFooter = scatter_footer,
                                      best_train = best_train,
                                      best_val = best_val,
                                      lineTitle = line_title,
                                      lineFooter = line_footer, stop = FALSE))
    Sys.sleep(.1)
    parents <- models[index_kept]
    models <- parents

    for (j in 1:remaining) {

      couple <- sample(parents, size = 2)
      mother <- couple[[1]]
      father <- couple[[2]]
      child <- .breed(mother, father, hypers, bg4test, bg_folds,
                      mutation_chance)
      train_metric[kept + j, ] <- list(kept + j,
                                       .get_metric(metric, child, env = env,
                                                   parallel = parallel))
      if (metric != "aicc")
        val_metric[kept + j, ] <- list(kept + j, .get_metric(metric, child,
                                                             test))
      scatter_footer[kept + j] <- .get_model_hyperparams(child)

      models <- c(models, child)
      .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                        gen = i, scatterFooter = scatter_footer,
                                        best_train = best_train,
                                        best_val = best_val,
                                        lineTitle = line_title,
                                        lineFooter = line_footer, stop = FALSE))
      Sys.sleep(.1)
      pb$tick(1)
    }
    metrics <- list(train_metric$y, val_metric$y)
    rank_index <- .get_rank_index(metric, metrics)

    if (!is.logical(rank_index)) {
      models <- models[rank_index]
      train_metric <- data.frame(x = seq(1, pop), y = metrics[[1]][rank_index])
      val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])
      scatter_footer <- scatter_footer[rank_index]
      best_train[i + 2] <- train_metric[1, 2]
      if (metric != "aicc")
        best_val[i + 2] <- val_metric[1, 2]
      line_title <- c(line_title, paste("Generation", i))
      line_footer <- c(line_footer, .get_model_hyperparams(models[[1]]))
      .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                        gen = i, scatterFooter = scatter_footer,
                                        best_train = best_train,
                                        best_val = best_val,
                                        lineTitle = line_title,
                                        lineFooter = line_footer, stop = FALSE))
      Sys.sleep(.1)
    } else {
      stop(paste("Optimization algorithm interrupted at generation", i,
                 "because it overfits validation dataset!"))
    }
  }

  .update_chart(folder, data = list(train = train_metric, val = val_metric,
                                    gen = i, scatterFooter = scatter_footer,
                                    best_train = best_train,
                                    best_val = best_val,
                                    lineTitle = line_title,
                                    lineFooter = line_footer, stop = TRUE))
  output <- .create_optimise_output(models, metric, train_metric, val_metric)
  pb$tick(1)

  return(output)
}

.create_random_model <- function(model, settings, bg4test, bg_folds) {

  args <- .get_train_args(model)
  args[names(settings)] <- settings
  if (!is.null(bg_folds)) {
    bg <- bg4test
    bg@data <- bg4test@data[bg_folds[1:settings$bg], ]
    bg@coords <- bg4test@coords[bg_folds[1:settings$bg], ]
    row.names(bg@data) <- NULL
    row.names(bg@coords) <- NULL
    args$bg <- bg
  }

  random_model <- do.call("train", args)

  return(random_model)
}

.breed <- function(mother, father, hypers, bg4test, bg_folds, mutation_chance) {

  mother_args <- .get_train_args(mother)
  model_args <- mother_args
  father_args <- .get_train_args(father)
  for (arg in names(hypers)) {
    model_args[[arg]] <- sample(c(mother_args[[arg]], father_args[[arg]]),
                                size = 1)[[1]]
  }

  if (mutation_chance > runif(1)) {
    # Only hypers with more than two values can be use for mutation
    mutation <- sample(names(hypers)[lengths(hypers) > 2], size = 1)
    if (mutation != "bg") {
      options <- setdiff(hypers[[mutation]], c(mother_args[[mutation]],
                                               father_args[[mutation]]))
      model_args[[mutation]] <- sample(options, size = 1)
    } else {
      options <- setdiff(hypers[[mutation]],
                         c(nrow(mother_args[[mutation]]@data),
                           nrow(father_args[[mutation]]@data)))
      n_bgs <- sample(options, size = 1)
      bg <- bg4test
      bg@data <- bg4test@data[bg_folds[1:n_bgs], ]
      bg@coords <- bg4test@coords[bg_folds[1:n_bgs], ]
      row.names(bg@data) <- NULL
      row.names(bg@coords) <- NULL
      model_args$bg <- bg
    }
  }

  new_model <- do.call("train", model_args)

  return(new_model)
}
