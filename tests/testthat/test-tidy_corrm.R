context("Tests of tidy_corrm() functionalities")

# prepare dataset for testing
ldros <- dplyr::mutate_if(drosera, is.numeric, log)

# Preprocessing ---------------------------------------------------------------
test_that("matrix is correctly coerced to data frame", {

  expect_s3_class(
    TidyCorrm$preprocess_data(data = matrix(rnorm(21), ncol = 3), arg = list()),
    "data.frame"
  )

})

test_that("Infinite values and NaN are handled correctly", {

  expect_false({
    dat <- TidyCorrm$preprocess_data(data = data.frame(x = rep(NaN, 10), y = Inf), arg = list())
    any(corrmorant:::is.nan.data.frame(dat$x) | corrmorant:::is.infinite.data.frame(dat$y))
  })

})

# Reshaping ---------------------------------------------------------------
test_that("Reshape_data output has right dimensions", {

  test_out <- TidyCorrm$reshape_data(
    ldros,
    arg = list(labels = NULL,
               rescale = "by_sd",
               corr_group = NULL,
               corr_method = "pearson")
  )

  expect_equal(
    dim(test_out),
    c(sum(sapply(ldros, is.numeric)) ^ 2 * nrow(ldros),  7 + 2)
  )

})

test_that("Correlation coefficients calculated correctly", {

  corrs <- tidy_corrm(ldros[1:20, ]) %>%
    dplyr::select(var_x, var_y, .corr) %>%
    dplyr::filter(!duplicated(.)) %>%
    tidyr::spread(key = var_y, value = .corr) %>%
    dplyr::select(-var_x) %>%
    as.matrix() %>%
    magrittr::set_rownames(colnames(.))

  expect_equal(
    corrs,
    cor(dplyr::select_if(ldros[1:20, ], is.numeric))
  )
})

test_that("Correct errors and messages are issued", {

  # large number of rows
  expect_message(
    tidy_corrm(matrix(rnorm(100), ncol = 20)),
    regexp = "Plotting may take very long"
  )

  # large skew
  expect_message(
    tidy_corrm(matrix(c(rnorm(100),rep(100, 5)), ncol = 5)),
    regexp = "Some variables are highly skewed"
  )

  # wrong number of labels
  expect_error(
    tidy_corrm(matrix(c(rnorm(12), ncol = 3)), labels = letters[1:10]),
    regexp = "Number of labels"
  )

})

test_that("Labeling works", {
  data <- data.frame(x = 1:10,
                     y = 10:1,
                     z = rep(3, 10))
  expect_equal(
    unique(tidy_corrm(data, labels = letters[1:3])$var_x),
    factor(letters[1:3], ordered = TRUE)
  )

})

test_that("Rescaling works", {

  data <- matrix(rnorm(50), ncol = 2)

  t1 <- tidy_corrm(data)
  t2 <- tidy_corrm(data, rescale = "by_range")
  t3 <- tidy_corrm(data, rescale = "as_is")

  expect_equal(range(dplyr::filter(t1, var_x == "V1", var_y == "V1")$x),
               range(scale(data[,1]))
               )

  expect_equal(range(dplyr::filter(t2, var_x == "V1", var_y == "V1")$x),
               c(0, 1)
               )

  expect_equal(range(dplyr::filter(t3, var_x == "V1", var_y == "V1")$x),
               range(data[,1])
               )

})

# Post-processing -------------------------------------------------------------
test_that("Mutating works", {

  data <- tidy_corrm(
    ldros,
    mutates = quos(
      organ = ifelse(substr(var_x, 1, 1) == "p", "petiole", "leaf"),
      dimension = ifelse(grepl("width", var_x), "width", "length")
      )
    )

  expect_equal(unique(data$organ), c("petiole", "leaf"))
  expect_equal(unique(data$dimension), c("length", "width"))

})

# Use of method ----------------------------------------------------------------
test_that("tidy_corrm fails for wrong object types", {

  expect_error(
    tidy_corrm(var),
    regexp = "data must be a data.frame or matrix"
    )

})

test_that("Misspecified mutates cause correct error", {

  expect_error(
    tidy_corrm(ldros, mutates = ggcorrm),
    regexp = "must be a named list of quosures"
  )

})
