skip_on_cran()

data <- SDMtune:::t

datasets <- trainValTest(data,
                         val = 0.2,
                         test = 0.2,
                         only_presence = TRUE,
                         seed = 61516)
train <- datasets[[1]]
val <- datasets[[2]]

# Train a model
model <- train("Maxnet",
               data = train)

mother <- SDMtune:::bm_maxnet
father <- train("Maxnet",
                data = data,
                fc = "l",
                reg = 2)

h <- list(fc = c("l", "lq", "lqph"),
          reg = c(1, 2))

metrics <- list(c(10, 11, 12), c(8, 10, 13))

test_that("The interactive chart is not created", {
  o <- optimizeModel(model,
                     hypers = h,
                     metric = "auc",
                     test = val,
                     pop = 3,
                     gen = 1,
                     interactive = FALSE,
                     progress = FALSE)

  expect_false(any(grepl("SDMtune-optimizeModel", list.dirs(tempdir()))))
})

test_that("The output is corrects and crates the interactive chart", {
  o <- optimizeModel(model,
                     hypers = h,
                     metric = "auc",
                     test = val,
                     pop = 3,
                     gen = 1)

  expect_s4_class(o, "SDMtune")
  expect_true(any(grepl("SDMtune-optimizeModel", list.dirs(tempdir()))))
})

test_that("Exception are raised", {
  # keep_best + keep_random > 1
  expect_snapshot_error(optimizeModel(mother,
                                      hypers = h,
                                      metric = "auc",
                                      test = data,
                                      keep_best = 0.6,
                                      keep_random = 0.6,
                                      pop = 3))

  # Only one hyperparameter
  expect_snapshot_error(optimizeModel(mother,
                                      hypers= list(fc = "l"),
                                      metric = "auc",
                                      test = data))

  # All hyperparameters with only 1 value
  expect_snapshot_error(optimizeModel(mother,
                                      hypers = list(fc = "l", reg = 1),
                                      metric = "auc",
                                      test = data))

  # Less models than population size
  expect_snapshot_error(optimizeModel(mother,
                                      hypers = h,
                                      metric = "auc",
                                      test = data,
                                      pop = 7))

  # Number of models equal to population size
  expect_snapshot_error(optimizeModel(mother,
                                      hypers = h,
                                      metric = "auc",
                                      test = data,
                                      pop = 6))

  # Overfit validation dataset at generation 0
  expect_snapshot_error(optimizeModel(mother,
                                      hypers = h,
                                      metric = "auc",
                                      test = data,
                                      pop = 3))
})

test_that("Crossover is executed", {
  set.seed(30, kind = "Mersenne-Twister", sample.kind = "Rejection")
  x <- .breed(mother, father, h, mutation_chance = 0)

  # fc comes from father
  expect_equal(x@model@fc, father@model@fc)

  # reg comes from mother
  expect_equal(x@model@reg, mother@model@reg)
})

test_that("Mutation is executed", {
  # For an hyperparameter different from a
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  x <- .breed(mother, father, h, mutation_chance = 1)

  # fc comes from mutation
  expect_equal(x@model@fc, "lq")
})

test_that("The rank is correct", {
  # For AICc the most important is the one with the lowest metric
  expect_equal(.get_rank_index("aicc", metrics), c(1, 2, 3))

  # For AUC or TSS the most important is the one with the highest value not
  # overfitting
  expect_equal(.get_rank_index("auc", metrics), c(2, 1, 3))

  # All model are overfitting
  metrics <- list(c(10, 11, 12), c(11, 12, 13))
  expect_false(.get_rank_index("auc", metrics))
})

test_that("The function raises errors", {
  expect_snapshot_error(optimizeModel(model,
                                      hypers = h,
                                      metric = "aicc",
                                      test = val,
                                      env = "spam",
                                      pop = 3,
                                      gen = 1,
                                      interactive = FALSE,
                                      progress = FALSE))
})

# TODO: Remove with version 2.0.0
test_that("The function raises an error if a raster object is used", {
  x <- integer(1)
  class(x) <- "Raster"
  expect_snapshot_error(optimizeModel(model,
                                      hypers = h,
                                      metric = "aicc",
                                      test = val,
                                      env = x,
                                      pop = 3,
                                      gen = 1,
                                      interactive = FALSE,
                                      progress = FALSE))
})
