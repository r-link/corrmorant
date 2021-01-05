context("Tests of geom_relpoint()")

# create minimal dataset for data with different ranges
d_abs <- matrix(c(rep(0, 3), 1:3), nrow = 2, byrow = TRUE)
# create relative coordinates to add to each facet
d_rel <- tibble(x = 2:8 / 10, y = 8:2 / 10)

# creat two analogous plots with different ranges
p1 <- ggcorrm(d_abs) +
  geom_relpoint(aes(relx = x, rely = y),
                data = d_rel, inherit.aes = FALSE)
p2 <- ggcorrm(2 * d_abs) +
  geom_relpoint(aes(relx = x, rely = y),
                data = d_rel, inherit.aes = FALSE)

# build plots
build1 <- ggplot_build(p1)
build2 <- ggplot_build(p2)

# get function to extract ranges
rangefun <- function(build) {
  sapply(c(build$layout$panel_scales_x,
           build$layout$panel_scales_y),
         function(x) x$range$range)
}

test_that("geom_relpoint returns the same relative coordinates independent of the range of the data", {
  # test if range of p2 differs by the range of p1 by a factor of two
  expect_equal(rangefun(build1), rangefun(build2) / 2)

  # test if relative coordinates are identical
  expect_equal(build1$data[[2]], build2$data[[2]]
)
})
