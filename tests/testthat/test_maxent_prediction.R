context("Maxent Prediction")

model <- SDMtune:::bm_maxent
maxent_model <- SDMmodel2MaxEnt(model)
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
env <- raster::stack(files)
p <- p_cont <- p_cat <- SDMtune:::p
bg <- SDMtune:::bg_model
bg_cont <- bg_cat <- SDMtune:::bg_model
# Remove categoriacal variable
p_cont@data <- p@data[, 1:7]
bg_cont@data <- bg@data[, 1:7]
# Remove continuous variables
p_cat@data <- p@data[, 8, drop = FALSE]
bg_cat@data <- bg@data[, 8, drop = FALSE]

skip_if(Sys.getenv("R_COVR") == "true",
        message = "Skip Maxent Prediction tests with covr!")

test_that("The function predicts cloglog correctly", {
  expect_equal(predict(model, p, type = "cloglog"),
               predict(maxent_model, p@data),
               tolerance = 1e-7)
})

test_that("The function predicts logistic correctly", {
  expect_equal(predict(model, p, type = "logistic"),
               predict(maxent_model, p@data, args = "outputformat=logistic"),
               tolerance = 1e-7)
})

test_that("The function predicts raw correctly", {
  expect_equal(predict(model, p, type = "raw"),
               predict(maxent_model, p@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

test_that("The function predicts raster correctly", {
  expect_equal(predict(model, env, type = "cloglog"),
               predict(maxent_model, env, args = "outputformat=cloglog"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cont, bg_cont, fc = "l")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc l correctly", {
  expect_equal(predict(model, bg_cont, type = "raw"),
               predict(maxent_model, bg_cont@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cont, bg_cont, fc = "q")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc q correctly", {
  expect_equal(predict(model, bg_cont, type = "raw"),
               predict(maxent_model, bg_cont@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cont, bg_cont, fc = "p")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc p correctly", {
  expect_equal(predict(model, bg_cont, type = "raw"),
               predict(maxent_model, bg_cont@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cont, bg_cont, fc = "h")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc h correctly", {
  expect_equal(predict(model, bg_cont, type = "raw"),
               predict(maxent_model, bg_cont@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cont, bg_cont, fc = "t")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc t correctly", {
  expect_equal(predict(model, bg_cont, type = "raw"),
               predict(maxent_model, bg_cont@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", p_cat, bg_cat, fc = "t")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc categorical correctly", {
  expect_equal(predict(model, p_cat, type = "cloglog"),
               predict(maxent_model, p_cat@data),
               tolerance = 1e-7)
})
