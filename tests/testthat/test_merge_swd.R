context("Merge SWD")

test_that("The function produce the correct output", {
  x <- mergeSWD(SDMtune:::p, SDMtune:::p)
  expect_s4_class(x, "SWD")
  expect_equal(rownames(x@data), as.character(1:(nrow(SDMtune:::p@data) * 2)))
  expect_equal(rownames(x@coords), as.character(1:(nrow(SDMtune:::p@data) * 2)))
  expect_equal(x@species, SDMtune:::p@species)
})

test_that("The function raises errors", {
  expect_error(mergeSWD(SDMtune:::p, SDMtune:::p@data),
               "The function accepts only SWD objects!")
  x <- SDMtune:::p
  x@species <- "Gypaetus barbatus"
  expect_error(mergeSWD(x, SDMtune:::p),
               "SWD1 and SWS2 have a different spwcies!")
})

test_that("The function warns if datasets have different variables", {
  x <- SDMtune:::p
  x@data$biome <- NULL
  expect_warning(mergeSWD(x, SDMtune:::p))
  # Check that common columns are merged
  expect_named(suppressWarnings(mergeSWD(x, SDMtune:::p)@data), names(x@data))
})
