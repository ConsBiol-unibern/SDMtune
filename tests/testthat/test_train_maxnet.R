context("Train Maxnet")

skip_on_cran()

test_that("The function trainMaxent produces the correct ouput", {
  m <- trainMaxnet(SDMtune:::p, SDMtune:::bg_model, reg = 1.2, fc = "l")
  expect_s4_class(m, "SDMmodel")
  expect_s4_class(m@model, "Maxnet")
  expect_s4_class(m@p, "SWD")
  expect_s4_class(m@a, "SWD")
  expect_equal(m@model@reg, 1.2)
  expect_equal(m@model@fc, "l")
  expect_equal(m@p, SDMtune:::p)
  expect_equal(m@a, SDMtune:::bg_model)
})
