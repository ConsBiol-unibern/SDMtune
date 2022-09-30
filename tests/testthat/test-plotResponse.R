skip_on_cran()

m <- SDMtune:::bm_maxnet
m_cv <- SDMtune:::bm_maxnet_cv

test_that("Error are raised", {
  expect_error(plotResponse(m, "biowhat", "cloglog"),
               "Variable biowhat is not used to train the model.")
})

test_that("Labels and output are correct for SDMmodel objects", {
  p <- plotResponse(m,
                    var = "bio1",
                    type = "cloglog",
                    rug = TRUE,
                    marginal = TRUE)

  expect_length(p$layers, 3)  # line and two rugs
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "cloglog output")
  expect_true(min(p$data$y) >= 0)
  expect_true(max(p$data$y) <= 1)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomLine")

  p <- plotResponse(m,
                    var = "bio1",
                    type = "logistic",
                    marginal = FALSE)

  expect_length(p$layers, 1)  # only line
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "logistic output")
  expect_true(min(p$data$y) >= 0)
  expect_true(max(p$data$y) <= 1)
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomLine")
})

test_that("Labels and output are correct for SDMmodelCV objects", {
  p <- plotResponse(m_cv,
                    var = "bio1",
                    type = "exponential",
                    rug = TRUE,
                    marginal = TRUE)

  expect_length(p$layers, 4)  # line, ribbon and two rugs
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "exponential output")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomLine")
})

test_that("Labels and output are correct for categorical variables", {
  # SDMmodel object
  p <- plotResponse(m,
                    var = "biome",
                    type = "cloglog",
                    marginal = TRUE)

  expect_length(p$layers, 1)  # bars
  expect_equal(p$labels$x, "biome")
  expect_equal(p$labels$y, "cloglog output")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")

  # SDMmodelCV object
  p <- plotResponse(m_cv,
                    var = "biome",
                    type = "cloglog",
                    marginal = TRUE)

  expect_length(p$layers, 2)  # bars and error bars
  expect_equal(p$labels$x, "biome")
  expect_equal(p$labels$y, "cloglog output")
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomBar")
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomErrorbar")
})
