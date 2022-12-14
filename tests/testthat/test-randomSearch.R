skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)
h <- list(fc = c("l", "q", "p"),
          reg = 1:3)

test_that("The interactive chart is not created", {
  randomSearch(SDMtune:::bm_maxnet,
               hypers = h,
               metric = "aicc",
               pop = 3,
               env = predictors,
               interactive = FALSE,
               progress = FALSE,
               seed = 25)

  expect_false(any(grepl("SDMtune-randomSearch", list.dirs(tempdir()))))
})

test_that("randomSearch produces the expected output", {
  o <- randomSearch(SDMtune:::bm_maxnet,
                    hypers = h,
                    metric = "aicc",
                    pop = 3,
                    env = predictors,
                    progress = FALSE,
                    seed = 25)

  expect_s4_class(o, "SDMtune")
  expect_s4_class(o@models[[1]], "SDMmodel")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("fc", "reg", "AICc", "delta_AICc"))
  expect_length(o@models, 3)
  expect_equal(o@results$fc, c("q", "l", "l"))
  expect_equal(o@results$reg, c(2, 2, 3))
  expect_true(any(grepl("SDMtune-randomSearch", list.dirs(tempdir()))))
})
