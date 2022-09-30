vi <- data.frame(Variable = c("bio1", "bio5", "bio7"),
                 Percent_contribution = c(36.0, 12.4, 27.3))

test_that("Output is correct", {
  p <- plotVarImp(vi)

  expect_equal(p$labels$y, "Percent contribution")

  # The function orders the values
  expect_equal(p$data$Variable,
               factor(c("bio5", "bio7", "bio1"),
                      levels = c("bio5", "bio7", "bio1")))
  colnames(vi) <- c("Variable", "Permutation_importance")
})
