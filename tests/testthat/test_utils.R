context("Utils")

model <- SDMtune:::bm_maxnet
model_cv <- SDMtune:::bm_maxnet_cv
h <- list(fc = c("l", "lq", "lqp"), reg = seq(.2, 2., .2))

test_that(".get_model_class", {
  expect_equivalent(.get_model_class(model), "Maxnet")
  expect_equivalent(.get_model_class(model_cv), "Maxnet")
})

test_that(".get_model_reg", {
  expect_equal(.get_model_reg(model), 1)
  expect_equal(.get_model_reg(model_cv), 1)
})

test_that(".get_model_fc", {
  expect_equal(.get_model_fc(model), "lqph")
  expect_equal(.get_model_fc(model_cv), "lqph")
})

test_that("The function .get_hypers_grid generates the correct grid", {
  expect_type(.get_hypers_grid(model, h)$a, "integer")
})
