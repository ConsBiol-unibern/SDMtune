context("Maxent arguments")

test_that("The correct fc arguments are created", {
  expect_equal(.get_fc_args("lqpht"),
               c("noautofeature", "threshold"))
  expect_error(.get_fc_args("lb"))
  })
test_that("The correct arguments are created", {
  expect_equal(.make_args(1, "l", 500, "removeduplicates=false"),
               c("betamultiplier=1", "maximumiterations=500", "noautofeature",
                 "noquadratic", "noproduct", "nohinge",
                 "removeduplicates=false"))
  })
