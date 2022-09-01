test_that("The function is verbose", {
  expect_snapshot(checkMaxentInstallation(verbose = TRUE))
})

test_that("The function output a silent logical", {
  expect_true(
    is.logical(expect_silent(checkMaxentInstallation(verbose = FALSE))))
})
