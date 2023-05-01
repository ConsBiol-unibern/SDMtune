test_that("The function provides the expected output", {
  model <- SDMtune:::bm_maxnet_cv
  combined_model <- combineCV(model)
  expect_s4_class(combined_model, "SDMmodel")
  expect_equal(model@data, combined_model@data)
})
