context("Tests of stat_funtext")

test_that("stat_funtext works with a function", {
  p <- ggcorrm(drosera) +
    lotri(stat_funtext(
      aes(label = after_stat(fun_out)),
      fun = function(x, y) round(cor(x, y), 2)
    ))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("fun_out" %in% names(ld))
  # fun_out should be numeric correlation values
  expect_true(is.numeric(ld$fun_out))
  expect_true(all(ld$fun_out >= -1 & ld$fun_out <= 1))
})

test_that("stat_funtext works with a lambda formula", {
  p <- ggcorrm(drosera) +
    lotri(stat_funtext(
      aes(label = after_stat(fun_out)),
      fun = ~ length(.x)
    ))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("fun_out" %in% names(ld))
  expect_true(all(ld$fun_out > 0))
})

test_that("stat_funtext works with a quosure", {
  p <- ggcorrm(drosera) +
    lotri(stat_funtext(
      aes(label = after_stat(fun_out)),
      fun = rlang::quo(dplyr::n())
    ))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("fun_out" %in% names(ld))
  expect_true(all(ld$fun_out > 0))
})

test_that("stat_funtext rejects invalid fun argument", {
  expect_error(
    ggplot_build(
      ggcorrm(drosera) +
        lotri(stat_funtext(aes(label = after_stat(fun_out)), fun = "not_a_function"))
    ),
    "fun argument.*must be a function"
  )
})
