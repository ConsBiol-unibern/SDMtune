file1 <- tempfile(fileext = ".csv")
file2 <- tempfile(fileext = ".csv")
x <- SDMtune:::t

test_that("Function saves object in one file correctly", {
  withr::defer(unlink(file1))
  swd2csv(x, file_name = file1)
  f1 <- read.csv(file1)

  expect_true(file.exists(file1))
  expect_named(f1, c("Species", "pa", names(x@coords), names(x@data)))
})

test_that("Function saves object in two files correctly", {
  withr::defer(unlink(c(file1, file2)))
  expect_silent(swd2csv(x, file_name = c(file1, file2)))

  # Presence file
  f1 <- read.csv(file1)
  expect_true(file.exists(file1))
  expect_named(f1, c("Species", names(x@coords), names(x@data)))

  # Absence/background file
  f2 <- read.csv(file2)
  expect_true(file.exists(file2))
  expect_named(f2, c("Species", names(x@coords), names(x@data)))
})

test_that("The function raises errors", {
  expect_snapshot_error(
    swd2csv(x, file_name = "spam")
  )
})
