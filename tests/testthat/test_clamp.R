context("Clamp")

v <- c(1:9, 0)
data <- clamp(v, 3, 7)

test_that("The function clamps correctly", {
  expect_identical(data, c(3, 3, 3, 4, 5, 6, 7, 7, 7, 3))
  expect_equal(length(data), 10)
})

# test_that("Exceptions are thrown", {
#   #Function throws exception if called with a data frames as first argument
#   expect_that(clamp(as.data.frame(v), 3, 7), throws_error())
# })
