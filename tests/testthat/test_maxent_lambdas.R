context("Maxent lambdas")

file <- tempfile(fileext = ".lambdas")
lambdas <- data.frame(V1 = c("(biome=1.0)", "bio1", "linearPredictorNormalizer",
                             "densityNormalizer", "numBackgroundPoints",
                             "entropy"),
                      V2 = c(0.2, 0, 1.68, 21.5, 200, 5.8),
                      V3 = c(0.0, 5, NA, NA, NA, NA),
                      V4 = c(1.0, 6, NA, NA, NA, NA),
                      stringsAsFactors = FALSE)
write.table(lambdas, file = file, row.names = FALSE, col.names = FALSE,
            quote = FALSE, na = "", sep = ",")
lambdas <- .get_lambdas(file, SDMtune:::bg_model)

test_that("Lambda file is parsed correctly", {
  # Get the correct linear predictor normalizer
  expect_equal(lambdas$lpn, 1.68)
  # Get the correct density normalizer
  expect_equal(lambdas$dn, 21.50)
  # Get the correct entropy
  expect_equal(lambdas$entropy, 5.80)
  # Remove the braces from the feature
  expect_equal(lambdas$lambdas[1, 1], "biome=1.0")
  # Remove feature in which lambda is zero
  expect_length(lambdas$lambdas$feature, 1)
  # Lambdas has correct column names
  expect_named(lambdas$lambdas, c("feature", "lambda", "min", "max"))
  # Min max has the correct column names
  expect_named(lambdas$min_max, c("variable", "min", "max"))
  # Min max has the correct length
  expect_length(lambdas$min_max$variable, 1)
  # Output has the correct column names
  expect_named(lambdas, c("lambdas", "lpn", "dn", "entropy", "min_max"))
})

unlink(file)
