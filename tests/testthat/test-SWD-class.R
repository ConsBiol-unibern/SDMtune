test_that("Show method for SWD class produces the correct output", {
  # TODO rewrite the test when the new dataset is available
  p <- SDMtune:::p
  p@pa <- NA_real_
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
