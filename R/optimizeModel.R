#' Optimize Model
#'
#' The function uses a Genetic Algorithm implementation to optimize the model
#' hyperparameter configuration according to the chosen metric.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param hypers named list containing the values of the hyperparameters that
#' should be tuned, see details.
#' @param metric character. The metric used to evaluate the models, possible
#' values are: "auc", "tss" and "aicc".
#' @param test \linkS4class{SWD} object. Testing dataset used to evaluate
#' the model, not used with aicc and \linkS4class{SDMmodelCV} objects.
#' @param pop numeric. Size of the population.
#' @param gen numeric. Number of generations.
#' @param env \link[terra]{rast} containing the environmental variables, used
#' only with "aicc".
#' @param keep_best numeric. Percentage of the best models in the population to
#' be retained during each iteration, expressed as decimal number.
#' @param keep_random numeric. Probability of retaining the excluded models
#' during each iteration, expressed as decimal number.
#' @param mutation_chance numeric. Probability of mutation of the child models,
#' expressed as decimal number.
#' @param interactive logical. If `FALSE` the interactive chart is not created.
#' @param progress logical. If `TRUE` shows a progress bar.
#' @param seed numeric. The value used to set the seed to have consistent
#' results.
#'
#' @details To know which hyperparameters can be tuned you can use the output
#' of the function \link{getTunableArgs}. Hyperparameters not included in the
#' `hypers` argument take the value that they have in the passed model.
#'
#' An interactive chart showing in real-time the steps performed by the
#' algorithm is displayed in the Viewer pane.
#'
#' Part of the code is inspired by
#' \href{https://blog.coast.ai/lets-evolve-a-neural-network-with-a-geneticalgorithm-code-included-8809bece164}{this post}.
#'
#' @return \linkS4class{SDMtune} object.
#' @export
#'
#' @author Sergio Vignali
#'
#' @seealso \link{gridSearch} and \link{randomSearch}.
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          val = 0.2,
#'                          test = 0.2,
#'                          only_presence = TRUE,
#'                          seed = 61516)
#' train <- datasets[[1]]
#' val <- datasets[[2]]
#'
#' # Train a model
#' model <- train("Maxnet",
#'                data = train)
#'
#' # Define the hyperparameters to test
#' h <- list(reg = seq(0.2, 5, 0.2),
#'           fc = c("l", "lq", "lh", "lp", "lqp", "lqph"))
#'
#' # Run the function using as metric the AUC
#' \dontrun{
#' output <- optimizeModel(model,
#'                         hypers = h,
#'                         metric = "auc",
#'                         test = val,
#'                         pop = 15,
#'                         gen = 2,
#'                         seed = 798)
#' output@results
#' output@models
#' output@models[[1]]  # Best model
#' }
#' }
optimizeModel <- function(model,
                          hypers,
                          metric,
                          test = NULL,
                          pop = 20,
                          gen = 5,
                          env = NULL,
                          keep_best = 0.4,
                          keep_random = 0.2,
                          mutation_chance = 0.4,
                          interactive = TRUE,
                          progress = TRUE,
                          seed = NULL) {

  metric <- match.arg(metric, choices = c("auc", "tss", "aicc"))

  # Create data frame with all possible combinations of hyperparameters
  grid <- .get_hypers_grid(model, hypers)

  # Check that arguments are correctly provided
  .check_args(model, metric, test, env, hypers)
  # Check if at least two hyperparameters have more than one value
  .check_optimize_args(hypers, grid, pop, keep_best, keep_random)

  if (!is.null(env)) {
    # TODO: Remove with version 2.0.0
    if (inherits(env, "Raster")) {
      .raster_error("rast")
    }

    if (!inherits(env, "SpatRaster"))
      cli::cli_abort(c(
        "!" = "{.var env} must be a {.cls SpatRaster} object",
        "x" = "You have supplied a {.cls {class(env)}} instead."
      ))
  }

  if (inherits(model, "SDMmodelCV"))
    test <- TRUE

  kept_good <- round(pop * keep_best)
  kept_bad <- round(pop * keep_random)
  kept <- kept_good + kept_bad
  remaining <- pop - kept
  tot_models <- .get_total_models(pop, gen, remaining)
  algorithm <- ifelse(gen > 0, "Optimize Model", "Random Search")

  if (progress)
    cli::cli_progress_bar(
      name = algorithm,
      type = "iterator",
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | \\
                ETA: {cli::pb_eta} - {cli::pb_elapsed_clock}",
      total = tot_models + 1,
      clear = FALSE
    )

  if (!is.null(seed))
    set.seed(seed)

  models <- vector("list", length = pop)
  train_metric <- data.frame(x = NA_real_, y = NA_real_)
  val_metric <- data.frame(x = NA_real_, y = NA_real_)

  if (interactive) {
    best_train <- rep(NA_real_, gen + 2)
    best_val <- rep(NA_real_, gen + 2)
    best_train[1] <- .get_metric(metric, model, env = env)

    if (metric != "aicc") {
      best_val[1] <- .get_metric(metric, model, test = test)
    } else {
      best_val <- NULL
    }

    line_title <- "Starting model"
    line_footer <- .get_footer(model)
    scatter_footer <- vector("character", length = pop)
    chart_title <- ifelse(gen > 0, paste(algorithm, "- Generation 0"),
                          algorithm)
    folder_prefix <- ifelse(gen > 0,
                            "SDMtune-optimizeModel",
                            "SDMtune-randomSearch")

    settings <- list(pop = pop,
                     gen = gen,
                     metric = .get_metric_label(metric),
                     labels = c("start", 0:gen),
                     update = TRUE)

    data <- list(gen = 0,
                 best_train = best_train,
                 best_val = best_val,
                 title = chart_title,
                 lineTitle = line_title,
                 lineFooter = line_footer,
                 stop = FALSE)

    folder <- tempfile(folder_prefix)

    .create_chart(folder = folder, script = "optimizeModel.js",
                  settings = settings, data = data)
    .show_chart(folder, height = 500)
  }

  # Create data frame with all possible combinations of hyperparameters
  grid <- .get_hypers_grid(model, hypers)

  index <- sample(nrow(grid), size = pop)

  # Random search, create random population
  for (i in 1:pop) {

    models[[i]] <- .create_model_from_settings(model,
                                               grid[index[i], ])

    train_metric[i, ] <- list(i, .get_metric(metric, models[[i]], env = env))

    if (metric != "aicc")
      val_metric[i, ] <- list(i, .get_metric(metric, models[[i]], test))

    if (interactive) {
      scatter_footer[i] <- .get_footer(models[[i]])
      .update_data(folder, data = list(train = train_metric, val = val_metric,
                                       gen = 0, scatterFooter = scatter_footer,
                                       best_train = best_train,
                                       best_val = best_val,
                                       title = chart_title,
                                       lineTitle = line_title,
                                       lineFooter = line_footer, stop = FALSE))
    }

    if (progress)
      cli::cli_progress_update()
  }

  metrics <- list(train_metric$y, val_metric$y)
  rank_index <- .get_rank_index(metric, metrics)

  if (!is.logical(rank_index)) {
    models <- models[rank_index]
    train_metric <- data.frame(x = seq(1, pop), y = metrics[[1]][rank_index])
    val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])

    if (interactive) {
      scatter_footer <- scatter_footer[rank_index]
      best_train[2] <- train_metric[1, 2]

      if (metric != "aicc")
        best_val[2] <- val_metric[1, 2]

      line_title <- c(line_title, "Generation 0")
      line_footer <- c(line_footer, .get_footer(models[[1]]))
      .update_data(folder, data = list(train = train_metric, val = val_metric,
                                       gen = 0, scatterFooter = scatter_footer,
                                       best_train = best_train,
                                       best_val = best_val,
                                       title = chart_title,
                                       lineTitle = line_title,
                                       lineFooter = line_footer, stop = FALSE))
    }
  } else {
    cli::cli_abort(
      paste("Optimization algorithm interrupted at generation 0",
            "because it overfits validation dataset.")
    )
  }

  # Optimize using Genetic Algorithm
  if (gen > 0) {
    for (i in 1:gen) {
      index_kept <- c(1:kept_good, sample((kept_good + 1):pop, kept_bad))
      train_metric <- train_metric[index_kept, ]
      train_metric$x <- 1:kept

      if (metric != "aicc") {
        val_metric <- val_metric[index_kept, ]
        val_metric$x <- 1:kept
      }

        if (interactive) {
        chart_title <- paste("Genetic Algorithm - Generation", i)
        scatter_footer <- scatter_footer[index_kept]

        .update_data(folder, data = list(train = train_metric,
                                         val = val_metric,
                                         gen = i,
                                         scatterFooter = scatter_footer,
                                         best_train = best_train,
                                         best_val = best_val,
                                         title = chart_title,
                                         lineTitle = line_title,
                                         lineFooter = line_footer,
                                         stop = FALSE))
        }

      parents <- models[index_kept]
      models <- parents

      for (j in 1:remaining) {

        couple <- sample(parents, size = 2)
        mother <- couple[[1]]
        father <- couple[[2]]
        child <- .breed(mother, father, hypers, mutation_chance)
        train_metric[kept + j, ] <- list(kept + j,
                                         .get_metric(metric, child, env = env))
        if (metric != "aicc")
          val_metric[kept + j, ] <- list(kept + j, .get_metric(metric, child,
                                                               test))

        models <- c(models, child)

        if (interactive) {
          scatter_footer[kept + j] <- .get_footer(child)
          .update_data(folder, data = list(train = train_metric,
                                           val = val_metric, gen = i,
                                           scatterFooter = scatter_footer,
                                           best_train = best_train,
                                           best_val = best_val,
                                           title = chart_title,
                                           lineTitle = line_title,
                                           lineFooter = line_footer,
                                           stop = FALSE))
        }

        if (progress)
          cli::cli_progress_update()
      }

      metrics <- list(train_metric$y, val_metric$y)
      rank_index <- .get_rank_index(metric, metrics)

      if (!is.logical(rank_index)) {
        models <- models[rank_index]
        train_metric <- data.frame(x = seq(1, pop),
                                   y = metrics[[1]][rank_index])
        val_metric <- data.frame(x = seq(1, pop), y = metrics[[2]][rank_index])

        if (interactive) {

          if (metric != "aicc")
            best_val[i + 2] <- val_metric[1, 2]

          scatter_footer <- scatter_footer[rank_index]
          best_train[i + 2] <- train_metric[1, 2]
          line_title <- c(line_title, paste("Generation", i))
          line_footer <- c(line_footer, .get_footer(models[[1]]))
          .update_data(folder, data = list(train = train_metric,
                                           val = val_metric, gen = i,
                                           scatterFooter = scatter_footer,
                                           best_train = best_train,
                                           best_val = best_val,
                                           title = chart_title,
                                           lineTitle = line_title,
                                           lineFooter = line_footer,
                                           stop = FALSE))
        }
      } else {
        cli::cli_abort(
          paste("Optimization algorithm interrupted at generation", i,
                "because it overfits validation dataset.")
        )
      }
    }
  }

  if (interactive) {
    .update_data(folder, data = list(train = train_metric, val = val_metric,
                                     gen = i, scatterFooter = scatter_footer,
                                     best_train = best_train,
                                     best_val = best_val,
                                     title = chart_title,
                                     lineTitle = line_title,
                                     lineFooter = line_footer, stop = TRUE))
  }

  output <- .create_sdmtune_output(models, metric, train_metric, val_metric)

  if (progress)
    cli::cli_progress_update()

  output
}

.breed <- function(mother,
                   father,
                   hypers,
                   mutation_chance) {

  mother_args <- .get_train_args(mother)
  model_args <- mother_args
  father_args <- .get_train_args(father)

  # Crossover
  for (arg in names(hypers)) {
    model_args[[arg]] <- sample(c(mother_args[[arg]], father_args[[arg]]),
                                size = 1)[[1]]
  }

  # Mutation
  if (mutation_chance > stats::runif(1)) {
    # Only hypers with more than two values can be use for mutation
    mutation <- sample(names(hypers)[lengths(hypers) > 2], size = 1)
    options <- setdiff(hypers[[mutation]], c(mother_args[[mutation]],
                                             father_args[[mutation]]))
    model_args[[mutation]] <- ifelse(length(options) > 1,
                                     sample(options, size = 1), options)
  }

  if (inherits(mother, "SDMmodelCV"))
    model_args$progress <- FALSE

  do.call("train", model_args)
}

.check_optimize_args <- function(hypers,
                                 grid,
                                 pop,
                                 keep_best,
                                 keep_random) {

  if (keep_best + keep_random > 1)
    cli::cli_abort(
      "Sum of {.var keep_best} and {.var keep_random} cannot be more than 1."
    )

  if (length(names(hypers)) < 2) {
    cli::cli_abort(c(
      "!" = "You must provide at least two hyperparameters to be tuned",
      "i" = "Use {.fn gridSearch} to tune only one parameter."))
  }

  if (all(lengths(hypers) < 2))
    cli::cli_abort(
      paste("One hyperparameter in hypers should have more than two values to ",
            "allow crossover.")
    )

  # Check if possible random combinations <= pop
  if (nrow(grid) < pop) {
    cli::cli_abort(c(
      "!" = "Number of possible random models is lewer than population size",
      "i" = "Add more values to the {.var hyper} argument."))
  } else if (nrow(grid) == pop) {
    cli::cli_abort(c(
      "!" = "Number of possible random models is the same than population size",
      "i" = "Use {.fn gridSearch} function."))
  }
}

.get_rank_index <- function(metric,
                            metrics) {
  if (metric == "aicc") {
    # The best model is the one with the lowest AICc
    index <- order(metrics[[1]])
  } else {
    # The best model is the one with the highest AUC or TSS
    # Check if the models are all overfitting the validation dataset
    diff_metric <- metrics[[1]] - metrics[[2]]
    if (!any(diff_metric > 0))
      return(FALSE)
    # Ordered index of decreasing validation metric
    o <- order(-metrics[[2]])
    # Good models are those not overfitting the validation dataset
    good_models <- o[o %in% which(diff_metric > 0)]
    # Bad models have diff_metric >= 0
    bad_models <- o[!o %in% good_models]
    # Ordered index of decreasing diff_metric
    odm <- order(-diff_metric)
    # Ordered index of bad_models from the one less overfitting
    bad_models <- odm[odm %in% bad_models]
    # Combine indexes
    index <- c(good_models, bad_models)
  }

  index
}
