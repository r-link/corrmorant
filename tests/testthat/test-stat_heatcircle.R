context("Tests of stat_heatcircle and geom_heatcircle")

test_that("stat_heatcircle builds without error", {
  p <- ggcorrm(drosera) + lotri(stat_heatcircle(aes(fill = after_stat(corr))))
  pb <- ggplot_build(p)
  # layer data should exist and have corr column
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  # corr values should be between -1 and 1
  expect_true(all(ld$corr >= -1 & ld$corr <= 1))
})

test_that("stat_heatcircle returns one row per group", {
  p <- ggcorrm(drosera) + lotri(stat_heatcircle(aes(fill = after_stat(corr))))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  # stat now outputs one row per group (circle coords computed in geom)
  panel_group_counts <- as.data.frame(table(ld$PANEL, ld$group))
  expect_true(all(panel_group_counts$Freq[panel_group_counts$Freq > 0] == 1))
})

test_that("geom_heatcircle validates rmin and rmax at render time", {
  # validation now happens in geom's draw_panel, triggered by rendering
  expect_error(
    grid::grid.force(
      ggplotGrob(ggcorrm(drosera) + lotri(stat_heatcircle(rmin = -0.1)))
    ),
    "rmin and rmax must be between 0 and 1"
  )
  expect_error(
    grid::grid.force(
      ggplotGrob(ggcorrm(drosera) + lotri(stat_heatcircle(rmax = 1.5)))
    ),
    "rmin and rmax must be between 0 and 1"
  )
  expect_error(
    grid::grid.force(
      ggplotGrob(ggcorrm(drosera) + lotri(stat_heatcircle(rmin = 0.8, rmax = 0.2)))
    ),
    "rmin.*larger than rmax"
  )
})

test_that("lotri_heatcircle convenience function works", {
  p <- ggcorrm(drosera) + lotri_heatcircle()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  expect_true(nrow(ld) > 0)
})

test_that("scale_by rejects invalid values", {
  expect_error(
    ggcorrm(drosera) + lotri(stat_heatcircle(scale_by = "invalid")),
    "must be one of"
  )
})
