t <- SDMtune:::t

test_that("Both presence and absence locations are merged", {
  x <- mergeSWD(t, t)
  np <- nrow(t@data[t@pa == 1, ])
  na <- nrow(t@data[t@pa == 0, ])
  n <- (np * 2) + (na * 2)
  expect_s4_class(x, "SWD")
  expect_equal(rownames(x@data), as.character(1:n))
  expect_equal(rownames(x@coords), as.character(1:n))
  expect_equal(nrow(x@data), n)
  expect_equal(nrow(x@coords), n)
  expect_equal(x@species, t@species)
  expect_equal(x@pa, c(rep(1, np * 2), rep(0, na * 2)))
})

test_that("Only presence locations are merged if only_presence is TRUE", {
  x <- mergeSWD(t, t, only_presence = TRUE)
  np <- nrow(t@data[t@pa == 1, ])
  na <- nrow(t@data[t@pa == 0, ])
  n <- (np * 2) + na
  expect_s4_class(x, "SWD")
  expect_equal(rownames(x@data), as.character(1:n))
  expect_equal(rownames(x@coords), as.character(1:n))
  expect_equal(nrow(x@data), n)
  expect_equal(nrow(x@coords), n)
  expect_equal(x@species, t@species)
  expect_equal(x@pa, c(rep(1, np *  2), rep(0, na)))
})

test_that("The function raises errors", {
  expect_snapshot(mergeSWD(t, t@data), error = TRUE)
  x <- t
  x@species <- "Gypaetus barbatus"
  expect_snapshot(mergeSWD(x, t), error = TRUE)
})

test_that("The function warns if datasets have different variables", {
  x <- t
  x@data$biome <- NULL
  expect_message(mergeSWD(x, t),
                 paste("! The two SWD objects have different columns,",
                       "only the common columns are used in the merged object"))
  # Check that common columns are merged
  expect_named(suppressMessages(mergeSWD(x, t)@data), names(x@data))
})
