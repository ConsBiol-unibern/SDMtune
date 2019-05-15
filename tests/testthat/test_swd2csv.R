context("SWD to CSV")

file <- tempfile(fileext = ".csv")
swd2csv(SDMtune:::p, file)
f <- read.csv(file)

test_that("Function produces the correct output", {
  expect_true(file.exists(file))
  expect_named(f, c("Species", names(SDMtune:::p@coords),
                                     names(SDMtune:::p@data)))
})

teardown(unlink(file))
