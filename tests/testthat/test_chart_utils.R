context("Chart Utils")

model <- SDMtune:::bm_maxent
h <- list("fc" = c("l", "lq", "lqp"), "reg" = seq(.2, 2., .2))

test_that("The function .get_hypers_grig generates the correct grid", {
  expect_type(.get_hypers_grid(model, h)$a, "integer")
})
