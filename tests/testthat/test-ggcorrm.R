context("Tests of ggcorrm() functionalities")

test_that("ggcorrm handles data types correctly", {
  # fails with correct error when supplied with wrong data type
  expect_error(
    ggcorrm(lm),
    regexp = "data must be a data.frame or matrix."
  )

  # returns right data class if supplied with right data type
  expect_s3_class(
    ggcorrm(iris),
    class = "ggcorrm"
  )
  expect_s3_class(
    ggcorrm(matrix(rnorm(120), nrow = 40)),
    class = "ggcorrm"
  )
  expect_s3_class(
    ggcorrm(iris)$data,
    class = "tidy_corrm"
  )
})

test_that("ggcorrm passes on correct arguments", {
  # correct aesthetics passed on
  expect_equal(
    ggcorrm(iris, aes(col = Species))$mapping,
    aes(x = x, y = y, col = Species)
  )

  # right method passed on from external object
  dat <- tidy_corrm(iris, corr_method = "kendall")
  expect_equal(ggcorrm(dat)$plot_param$corr_method,
               "kendall"
  )

  # throws warning if supplied with ignored parameters
  expect_warning(
    ggcorrm(dat, rescale = "as_is"),
    regexp = "*arguments are ignored*"
  )

  # 'mutates' passed on correctly
  plot1 <- ggcorrm(
    iris, mutates = quos(z = substr(Species, 1, 4)))
  expect_equal(plot1$data$z,
               substr(dat$Species, 1, 4)
  )

  # aesthetics set correctly
  expect_equal(
    plot1$mapping,
    aes(x, y)
  )
})

test_that("make_corrm_layers produces right output", {
  # prepare function for testing
  layerfun <- function(layers){
    l<- corrmorant:::make_corrm_layers(layers)
    list(length(l),
         sapply(l, function(x) class(x$geom)[[1]] )
    )
  }

  # only dimensions
  expect_equal(
    layerfun(list(NULL, NULL, NULL)),
    list(1, c("GeomBlank"))
  )
  # diagonal backgrounds
  expect_equal(
    layerfun(list(1, NULL, NULL)),
    list(2, c("GeomBlank", "GeomRect"))
  )
  # diagonal + lower
  expect_equal(
    layerfun(list(1, 2, NULL)),
    list(3, c("GeomBlank", "GeomRect", "GeomRect"))
  )
  # all backgrounds
  expect_equal(
    layerfun(list(1, 2, 1)),
    list(4, c("GeomBlank", "GeomRect", "GeomRect", "GeomRect"))
  )
})
