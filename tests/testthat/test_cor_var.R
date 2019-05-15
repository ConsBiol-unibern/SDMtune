context("Correlated Variables")

test_that("The function raises error", {
  expect_error(corVar(SDMtune:::p@data))
})

test_that("The function creates the correct output", {
  expect_type(corVar(SDMtune:::p), "list")
  expect_true(min(corVar(SDMtune:::p, cor_th = 0.8)$value) >= 0.8 )
})
