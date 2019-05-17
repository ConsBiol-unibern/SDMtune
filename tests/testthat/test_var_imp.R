context("Variable Importance")

test_that("The function produces the correct output", {
  x <- varImp(SDMtune:::bm_maxent, permut = 2)
  expect_named(x, c("Variable", "Permutation_importance", "sd"))
  expect_equal(class(x), "data.frame")
  expect_equal(nrow(x), ncol(SDMtune:::bm_maxent@p@data))
  expect_setequal(x$Variable, colnames(SDMtune:::bm_maxent@p@data))
  # Column sd is not present for only one permutation
  expect_named(varImp(SDMtune:::bm_maxent, permut = 1),
               c("Variable", "Permutation_importance"))
})
