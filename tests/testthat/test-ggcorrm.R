test_that("ggcorrm handles data types correctly", {
  # fails with correct error when supplied with wrong data type
  expect_error(ggcorrm(lm), regexp = "data must be a data.frame or matrix.")

  # returns right data class if supplied with right data type
  expect_s3_class(ggcorrm(iris), class = "ggcorrm")
  expect_s3_class(ggcorrm(matrix(rnorm(120), nrow = 40)), class = "ggcorrm")
  expect_s3_class(ggcorrm(iris)$data, class = "tidy_corrm")
})

test_that("ggcorrm passes on correct arguments", {
  # correct aesthetics passed on
  expect_equal(ggcorrm(iris, aes(col = Species))$mapping,
               aes(x = x, y = y, col = Species))

  # right method passed on from external object
  dat <- tidy_corrm(iris, corr_method = "kendall")
  expect_equal(ggcorrm(dat)$plot_param$corr_method,
               "kendall")

  # throws warning if supplied with ignored parameters
  expect_warning(ggcorrm(dat, rescale = "as_is"),
                 regexp = "*arguments are ignored*")

  # 'mutates' passed on correctly
  plot1 <- ggcorrm(iris, mutates = quos(z = substr(Species, 1, 4)))
  expect_equal(plot1$data$z, substr(dat$Species, 1, 4))


})
