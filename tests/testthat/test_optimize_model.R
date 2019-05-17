context("Optimize Model")

mother <- SDMtune:::bm_maxnet
father <- train("Maxnet", SDMtune:::p, SDMtune:::bg_model, fc = "l", reg = 2)
h <- list(fc = c("l", "lq", "lqph"), reg = c(1, 2))
metrics <- list(c(10, 11, 12), c(8, 10, 13))

test_that("Crossover is executed", {
  set.seed(30, kind = "Mersenne-Twister", sample.kind = "Rejection")
  x <- .breed(mother, father, h, bg4test = NULL, bg_folds = NULL,
              mutation_chance = 0)
  # fc comes from mother
  expect_equal(x@model@fc, father@model@fc)
  # reg comes from father
  expect_equal(x@model@reg, mother@model@reg)
})

test_that("Mutation is executed", {
  # For an hyperparameter different from a
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  x <- .breed(mother, father, h, bg4test = NULL, bg_folds = NULL,
              mutation_chance = 1)
  # fc comes from mutation
  expect_equal(x@model@fc, "lq")
  # For a
  h <- list(fc = c("l", "lqph"), reg = c(1, 2), a = c(5000, 5100, 5200))
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  x <- .breed(mother, father, h, bg4test = SDMtune:::bg,
              bg_folds = sample(1:nrow(SDMtune:::bg@data)), mutation_chance = 1)
  # a comes from mutation
  expect_equal(nrow(x@a@data), 5100)
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
