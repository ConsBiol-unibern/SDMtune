context("SDMmodel to Maxent")

test_that("The function creates the cirrect output", {
  expect_s4_class(SDMmodel2MaxEnt(SDMtune:::bm_maxent), "MaxEnt")
  expect_equal(SDMmodel2MaxEnt(SDMtune:::bm_maxent)@presence,
               SDMtune:::bm_maxent@p@data)
  expect_equal(SDMmodel2MaxEnt(SDMtune:::bm_maxent)@absence,
               SDMtune:::bm_maxent@a@data)
  expect_equal(SDMmodel2MaxEnt(SDMtune:::bm_maxent)@lambdas,
               SDMtune:::bm_maxent@model@lambdas)
  expect_true(SDMmodel2MaxEnt(SDMtune:::bm_maxent)@hasabsence)
})

test_that("The function raises errors", {
  expect_error(SDMmodel2MaxEnt(SDMtune:::bm_maxnet))
})
