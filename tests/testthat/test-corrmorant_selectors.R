context("corrmorant selectors")

# temporary dataset
testdat  <- tidy_corrm(iris)


test_that("update_data() works with globally defined data", {
  # lower triangle
  expect_equal(
    unique(corrmorant:::update_data(waiver(), target = "lotri")(testdat)$pos),
    "lotri"
  )
  # upper triangle
  expect_equal(
    unique(corrmorant:::update_data(waiver(), target = "utri")(testdat)$pos),
    "utri"
  )
  # plot diagonal
  expect_equal(
    unique(corrmorant:::update_data(waiver(), target = "dia")(testdat)$pos),
    "dia"
  )
})


test_that("update_data() works on tidy_corrm objects as layer data", {
  # lower triangle
  expect_equal(
    unique(corrmorant:::update_data(testdat, target = "lotri")(testdat)$pos),
    "lotri"
  )
  # upper triangle
  expect_equal(
    unique(corrmorant:::update_data(testdat, target = "utri")(testdat)$pos),
    "utri"
  )
  # plot diagonal
  expect_equal(
    unique(corrmorant:::update_data(testdat, target = "dia")(testdat)$pos),
    "dia"
  )
})
