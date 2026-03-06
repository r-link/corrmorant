context("Tests of geom_heatmap, geom_heatcircle, and geom_heatpoint")

test_that("geom_heatmap renders without error", {
  p <- ggcorrm(drosera) + lotri_heatmap() + scale_fill_corr()
  # build succeeds
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  # render succeeds
  expect_silent(ggplotGrob(p))
})

test_that("geom_heatcircle renders without error", {
  p <- ggcorrm(drosera) + lotri_heatcircle() + scale_fill_corr()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  expect_silent(ggplotGrob(p))
})

test_that("geom_heatpoint renders without error", {
  p <- ggcorrm(drosera) +
    utri_heatpoint(mapping = aes(colour = after_stat(corr))) +
    scale_color_corr()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  expect_silent(ggplotGrob(p))
})

test_that("utri variants work", {
  p <- ggcorrm(drosera) + utri_heatmap() + utri_heatcircle()
  pb <- ggplot_build(p)
  expect_true(nrow(pb$data[[2]]) > 0)
  expect_true(nrow(pb$data[[3]]) > 0)
})

test_that("geom_heatcircle validates rmin/rmax at render time", {
  p <- ggcorrm(drosera) + lotri(stat_heatcircle(rmin = -0.1))
  # build succeeds (validation is in geom, not stat)
  expect_silent(ggplot_build(p))
  # render fails with validation error
  expect_error(ggplotGrob(p), "rmin and rmax must be between 0 and 1")
})

test_that("geom_heatcircle validates rmin > rmax at render time", {
  p <- ggcorrm(drosera) + lotri(stat_heatcircle(rmin = 0.8, rmax = 0.2))
  expect_error(ggplotGrob(p), "rmin.*larger than rmax")
})
