skip_on_cran()

m <- SDMtune:::bm_maxent
m_cv <- SDMtune:::bm_maxent_cv

test_that("The function returns the expected output", {
  expect_true(auc(m) < 1)
  expect_true(is.numeric(auc(SDMtune:::bm_maxnet)))
  expect_true(auc(m_cv) < 1)
  expect_true(is.numeric(auc(m_cv)))
})

test_that("The function uses the testing dataset", {
  expect_true(auc(m) != auc(m, test = m_cv@models[[1]]@data))
  expect_true(auc(m_cv) > auc(m_cv, test = TRUE))
})

test_that("The function raises warnings and errors", {
  expect_error(auc(m_cv, SDMtune:::t),
               "\"test\" argument invalid for \"SDMmodelCV\" objects! Use TRUE")
  # TODO Remove next test with next release
  expect_warning(auc(m, a = SDMtune:::t),
                 "Argument \"a\" is deprecated and not used anymore, it ")
})
