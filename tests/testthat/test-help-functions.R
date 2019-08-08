test_that("Convert old SWD object works", {
  data <- SDMtune:::t
  p <- .subset_swd(data, fold = data@pa == 1)
  a <- .subset_swd(data, fold = data@pa == 0)
  expect_s4_class(swd <- old2NewSWD(p, a), "SWD")
  expect_equal(swd@species, p@species)
  expect_equal(nrow(swd@data), sum(nrow(p@data), nrow(a@data)))
  expect_equal(nrow(swd@coords), sum(nrow(p@coords), nrow(a@coords)))
  expect_equal(sum(swd@pa == 1), nrow(p@data))
  expect_equal(sum(swd@pa == 0), nrow(a@data))
  expect_equal(rownames(swd@data), as.character(1:nrow(swd@data)))
  expect_equal(rownames(swd@coords), as.character(1:nrow(swd@data)))
  a@data <- a@data[, 1:4]
  expect_error(old2NewSWD(p, a),
               "p and a have data with different number of columns.")
  a <- .subset_swd(data, fold = data@pa == 0)
  colnames(a@data) <- c("bio1", "bio12", "bio16", "bio17", "bio5",  "bio6",
                        "bio7",  "bio8", "bio")
  expect_error(old2NewSWD(p, a),
               "p and a have data with different colnames.")
})
