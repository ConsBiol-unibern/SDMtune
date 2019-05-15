context("Maxent variable importance")

test_that("Error are raised", {
  expect_error(maxentVarImp(SDMtune:::bm_maxent_cv),
               "Function not available for SDMmodelCV!")
  expect_error(maxentVarImp(SDMtune:::bm_maxnet))
})

test_that("The output is correct", {
  expect_type(maxentVarImp(SDMtune:::bm_maxent), "list")
  expect_named(maxentVarImp(SDMtune:::bm_maxent),
               c("Variable", "Percent_contribution", "Permutation_importance"))
  expect_equal(nrow(maxentVarImp(SDMtune:::bm_maxent)),
               ncol(SDMtune:::p@data))
})
