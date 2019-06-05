context("SDMtune classes")

test_that("Show method for SWD class produces the correct output", {
  p <- SDMtune:::p
  expect_output(print(p), "Object of class SWD", fixed = TRUE)
  expect_output(print(p), "Species: Vultur gryphus", fixed = TRUE)
  expect_output(print(p), "Locations: 510", fixed = TRUE)
  expect_output(print(p), "bio1 bio12 bio16 bio17 bio5 bio6 bio7 bio8",
                fixed = TRUE)
  expect_output(print(p), "biome")
  p@data <- p@data[, 1:3]
  expect_output(print(p), "Continuous: bio1 bio12 bio16", fixed = TRUE)
  expect_output(print(p), "Categorical: NA", fixed = TRUE)
  p@data <- SDMtune:::p@data[, "biome", drop = FALSE]
  expect_output(print(p), "Continuous: NA", fixed = TRUE)
  expect_output(print(p), "Categorical: biome", fixed = TRUE)
})

test_that("Show method for Maxent class produces the correct output", {
  m <- SDMtune:::bm_maxent@model
  expect_output(print(m), "Class     : Maxent", fixed = TRUE)
  expect_output(print(m), "Reg       : 1", fixed = TRUE)
  expect_output(print(m), "FCs       : lqph", fixed = TRUE)
  expect_output(print(m), "Iterations: 500", fixed = TRUE)
})

test_that("Show method for Maxnet class produces the correct output", {
  m <- SDMtune:::bm_maxnet@model
  expect_output(print(m), "Class: Maxnet", fixed = TRUE)
  expect_output(print(m), "Reg  : 1", fixed = TRUE)
  expect_output(print(m), "FCs  : lqph", fixed = TRUE)
})

test_that("Show method for SDMmodel class produces the correct output", {
  m <- SDMtune:::bm_maxent
  expect_output(print(m), "Object of class SDMmodel", fixed = TRUE)
  expect_output(print(m), "Method: Maxent", fixed = TRUE)
  expect_output(print(m), "Species: Vultur gryphus", fixed = TRUE)
  expect_output(print(m), "Presence locations: 510", fixed = TRUE)
  expect_output(print(m), "a: 5000", fixed = TRUE)
  expect_output(print(m), "fc: lqph", fixed = TRUE)
  expect_output(print(m), "reg: 1", fixed = TRUE)
  expect_output(print(m), "iter: 500", fixed = TRUE)
  expect_output(print(m), "bio1 bio12 bio16 bio17 bio5 bio6 bio7 bio8",
                fixed = TRUE)
  expect_output(print(m), "biome")
  m@p@data <- SDMtune:::p@data[, 1:3]
  expect_output(print(m), "Continuous: bio1 bio12 bio16", fixed = TRUE)
  expect_output(print(m), "Categorical: NA", fixed = TRUE)
  m@p@data <- SDMtune:::p@data[, "biome", drop = FALSE]
  expect_output(print(m), "Continuous: NA", fixed = TRUE)
  expect_output(print(m), "Categorical: biome", fixed = TRUE)
  m <- SDMtune:::bm_maxnet
  expect_output(print(m), "Object of class SDMmodel", fixed = TRUE)
  expect_output(print(m), "Method: Maxnet", fixed = TRUE)
  expect_output(print(m), "Species: Vultur gryphus", fixed = TRUE)
  expect_output(print(m), "Presence locations: 510", fixed = TRUE)
  expect_output(print(m), "a: 5000", fixed = TRUE)
  expect_output(print(m), "fc: lqph", fixed = TRUE)
  expect_output(print(m), "reg: 1", fixed = TRUE)
  expect_output(print(m), "bio1 bio12 bio16 bio17 bio5 bio6 bio7 bio8",
                fixed = TRUE)
  expect_output(print(m), "biome")
})

test_that("Show method for SDMmodelCV class produces the correct output", {
  m <- SDMtune:::bm_maxent_cv
  expect_output(print(m), "Object of class SDMmodelCV", fixed = TRUE)
  expect_output(print(m), "Method: Maxent", fixed = TRUE)
  expect_output(print(m), "Replicates: 4", fixed = TRUE)
  expect_output(print(m), "Species: Vultur gryphus", fixed = TRUE)
  expect_output(print(m), "Presence locations: 510", fixed = TRUE)
  expect_output(print(m), "a: 5000", fixed = TRUE)
  expect_output(print(m), "fc: lqph", fixed = TRUE)
  expect_output(print(m), "reg: 1", fixed = TRUE)
  expect_output(print(m), "iter: 500", fixed = TRUE)
  expect_output(print(m), "bio1 bio12 bio16 bio17 bio5 bio6 bio7 bio8",
                fixed = TRUE)
  expect_output(print(m), "biome")
  m@p@data <- SDMtune:::p@data[, 1:3]
  expect_output(print(m), "Continuous: bio1 bio12 bio16", fixed = TRUE)
  expect_output(print(m), "Categorical: NA", fixed = TRUE)
  m@p@data <- SDMtune:::p@data[, "biome", drop = FALSE]
  expect_output(print(m), "Continuous: NA", fixed = TRUE)
  expect_output(print(m), "Categorical: biome", fixed = TRUE)
  m <- SDMtune:::bm_maxnet_cv
  expect_output(print(m), "Object of class SDMmodelCV", fixed = TRUE)
  expect_output(print(m), "Method: Maxnet", fixed = TRUE)
  expect_output(print(m), "Replicates: 4", fixed = TRUE)
  expect_output(print(m), "Species: Vultur gryphus", fixed = TRUE)
  expect_output(print(m), "Presence locations: 510", fixed = TRUE)
  expect_output(print(m), "a: 5000", fixed = TRUE)
  expect_output(print(m), "fc: lqph", fixed = TRUE)
  expect_output(print(m), "reg: 1", fixed = TRUE)
  expect_output(print(m), "bio1 bio12 bio16 bio17 bio5 bio6 bio7 bio8",
                fixed = TRUE)
  expect_output(print(m), "biome")
})
