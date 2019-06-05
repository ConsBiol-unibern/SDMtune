context("Plot Cor")

p <- plotCor(SDMtune:::bg_model, cor_th = .8)

test_that("The plot has the correct labels", {
  expect_equal(p$plot_env$label, "Spearman's\ncoefficient")
  expect_equal(p$labels$x, "Var2")
  expect_equal(p$labels$y, "Var1")
})

test_that("The plot as the correct text on the tiles", {
  expect_true(sum(abs(p$layers[[2]]$data$value) >= .8) == nrow(p$layers[[2]]$data))
})
