context("Tests of corrmorant selectors")

# temporary dataset
testdat  <- tidy_corrm(dplyr::mutate_if(drosera, is.numeric, log))

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


test_that("update_data() works on user-specified data without tidycorrm columns", {
  dat1 <- tibble(x = 0, y = 0, label = "Text")
  expect_equal(
    nrow(corrmorant:::update_data(dat1, "lotri")(testdat)),
    6 # all 6 rows should be included in this case
  )
})



test_that("update_data() works on user-specified data with one tidycorrm columns", {
  dat2 <- tibble(var_x = "petiole_length",  x = 0, y = 0, label = "Text")
  expect_equal(
    nrow(corrmorant:::update_data(dat2, "lotri")(testdat)),
    3 # only the two rows containing petiole length should be included
  )
})


test_that("update_data() works on user-specified data with two tidycorrm columns", {
  dat3 <- tibble(var_x = "petiole_length", var_y = "blade_length",  x = 0, y = 0, label = "Text")
  expect_equal(
    nrow(corrmorant:::update_data(dat3, "lotri")(testdat)),
    1 # only one row should meet this conditions
  )
})

test_that("update_data() breaks when var_x or var_y contain variables that are not in data", {
  # var_x wrong
  dat4 <- tibble(var_x = "oriole_length", var_y = "blade_length",  x = 0, y = 0, label = "Text")
  expect_error(
    corrmorant:::update_data(dat4, "lotri")(testdat),
    regexp = "variable names missing"
    )

  # var_y wrong
  dat5 <- tibble(var_x = "petiole_length", var_y = "blade_strength",  x = 0, y = 0, label = "Text")
  expect_error(
    corrmorant:::update_data(dat5, "lotri")(testdat),
    regexp = "variable names missing"
    )
})

