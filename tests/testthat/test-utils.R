context("Tests of utility functions")

test_that("rescale_var works", {
  # rescale to 20% - 80% of the range from 0 to 20
  expect_equal(
    range(rescale_var(-10:10, 0.2, 0.8,
                      range = c(0, 20))),
    c(4, 16)
  )
  # append 100 to the range and test if max is still 16
  expect_false(
    max(rescale_var(-10:10, 0.2, 0.8,
                    range = c(0, 20),
                    append_x = 100)) ==  16
  )
  # rescale does nothing if range is equal
  expect_equal(
    1:20,
    rescale_var(1:20, 0, 1, c(1, 20))
    )
})


test_that("prepare_aes_corrm behaves correctly", {
  # no new aesthetics specified
  expect_equal(
    update_aes_corrm(new_aes = NULL),
    aes(x = x, y = y)
  )

  # new aesthetics specified
  expect_equal(
    update_aes_corrm(new_aes = aes(col = xyz)),
    aes(x = x, y = y, colour = xyz)
  )

  # wrong aesthetics specified
  expect_warning(
    update_aes_corrm(new_aes = aes(x = xyz))
  )
  expect_equal(
    suppressWarnings(update_aes_corrm(new_aes = aes(x = xyz))),
    aes(x = x, y = y)
  )

  # only one standard argument
  expect_equal(
    update_aes_corrm(new_aes = NULL, standard_aes = aes(x = x)),
    aes(x = x)
  )

  # additional standard argument
  expect_equal(
    update_aes_corrm(new_aes = NULL,
                     standard_aes = aes(x, y, col = abc)),
    aes(x = x, y = y, colour = abc)
  )

})
