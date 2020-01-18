context("Tests of stat_corrtext() and related functions")

# get function (for easier coding of tests)
get_corrtext_pos <- corrmorant:::get_corrtext_pos

# fake stats for testing
stats1 <- data.frame(x = 1, y = 2, PANEL = 1, group = 1, .corr = .7)
stats2 <- data.frame(x = 1, y = 2, PANEL = 1, group = 1:3, .corr = .7)
stats3 <- data.frame(x = 1, y = 2, PANEL = 1, group = 1:6, .corr = .7)

test_that("get_corrtext_pos() computes correct position for one observation", {

  expect_equal(
    get_corrtext_pos(stats1, nrow = NULL, ncol = NULL, squeeze = 0.7, xrange = 0:1, yrange = 1:2)[,1:2],
    data.frame(x = mean(0:1), y = mean(1:2))
  )

})

test_that("get_corrtext_pos() computes correct range for multiple observations", {

  # squeeze = 0.7
  pos1 <- get_corrtext_pos(stats2, nrow = NULL, ncol = NULL,
                           squeeze = 0.7, xrange = 0:1, yrange = 1:2)[,1:2]
  expect_equal(range(pos1$x), c(0.15, 0.85))
  expect_equal(range(pos1$y), c(1.15, 1.85))

  # squeeze = 0.5
  pos2 <- get_corrtext_pos(stats2, nrow = NULL, ncol = NULL,
                           squeeze = 0.5, xrange = 0:1, yrange = 1:2)[,1:2]
  expect_equal(range(pos2$x), c(0.25, 0.75))
  expect_equal(range(pos2$y), c(1.25, 1.75))

})


test_that("get_corrtext_pos() computes correct row and column positions", {

  # no nrow and ncol specified
  pos1 <- get_corrtext_pos(stats3, nrow = NULL, ncol = NULL,
                           squeeze = 1, xrange = c(1, 3), yrange = 1:2)[,1:2]
  expect_equal(pos1$x, rep(1:3, 2))
  expect_equal(pos1$y, rep(2:1, each = 3)
               )

  # nrow specified (as in standard settings)
  pos2 <- get_corrtext_pos(stats3, nrow = 2, ncol = NULL,
                           squeeze = 1, xrange = c(1, 3), yrange = 1:2)[,1:2]
  expect_equal(pos2$x, rep(1:3, 2))
  expect_equal(pos2$y, rep(2:1, each = 3)
               )

  # ncol specified (as in standard settings)
  pos3 <- get_corrtext_pos(stats3, nrow = NULL, ncol = 3,
                           squeeze = 1, xrange = c(1, 3), yrange = 1:2)[,1:2]
  expect_equal(pos3$x, rep(1:3, 2))
  expect_equal(pos3$y, rep(2:1, each = 3)
               )

  # both specified (as in standard settings)
  pos4 <- get_corrtext_pos(stats3, nrow = 2, ncol = 3,
                           squeeze = 1, xrange = c(1, 3), yrange = 1:2)[,1:2]
  expect_equal(pos4$x, rep(1:3, 2))
  expect_equal(pos4$y, rep(2:1, each = 3)
               )

  # error when misspecified
  expect_error(get_corrtext_pos(stats3, nrow = 3, ncol = 3, squeeze = 1,
                                xrange = c(1, 3), yrange = 1:2),
               regexp = "Check dimensions in stat_corrtext"
               )

  # different nrow specified
  pos5 <- get_corrtext_pos(stats3, nrow = 3, ncol = NULL,
                           squeeze = 1, xrange = 1:2, yrange = c(1, 3))[,1:2]
  expect_equal(pos5$x, rep(1:2, 3))
  expect_equal(pos5$y, rep(3:1, each = 2)
  )

})
