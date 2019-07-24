t <- SDMtune:::t

test_that("The function produce the correct output", {
  x <- mergeSWD(t, t)
  expect_s4_class(x, "SWD")
  expect_equal(rownames(x@data), as.character(1:(nrow(t@data) * 2)))
  expect_equal(rownames(x@coords), as.character(1:(nrow(t@data) * 2)))
  expect_equal(nrow(x@data), nrow(t@data) * 2)
  expect_equal(nrow(x@coords), nrow(t@data) * 2)
  expect_equal(x@species, t@species)
  expect_equal(x@pa, c(rep(1, sum(t@pa == 1) * 2), rep(0, sum(t@pa == 0) * 2)))
})

test_that("The function raises errors", {
  expect_error(mergeSWD(t, t@data), "The function accepts only SWD objects!")
  x <- t
  x@species <- "Gypaetus barbatus"
  expect_error(mergeSWD(x, t), "SWD1 and SWS2 have a different spwcies!")
})

test_that("The function warns if datasets have different variables", {
  x <- t
  x@data$biome <- NULL
  expect_warning(mergeSWD(x, t))
  # Check that common columns are merged
  expect_named(suppressWarnings(mergeSWD(x, t)@data), names(x@data))
})
