context("Tests of stat_dia_bin and stat_dia_names")

test_that("stat_dia_bin builds without error in ggcorrm", {
  p <- ggcorrm(drosera) + dia_histogram()
  pb <- ggplot_build(p)
  # histogram layer should be layer 2 (after blank)
  ld <- pb$data[[2]]
  expect_true(nrow(ld) > 0)
  expect_true("ymin" %in% names(ld))
  expect_true("ymax" %in% names(ld))
})

test_that("stat_dia_bin rescales y values within panel range", {
  p <- ggcorrm(drosera) + dia_histogram()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  # ymax should always be >= ymin
  expect_true(all(ld$ymax >= ld$ymin))
})

test_that("stat_dia_bin removes raw count columns", {
  p <- ggcorrm(drosera) + dia_histogram()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  # these columns should be removed by the stat
  expect_false("density" %in% names(ld))
  expect_false("ncount" %in% names(ld))
  expect_false("ndensity" %in% names(ld))
})

test_that("dia_names builds without error", {
  p <- ggcorrm(drosera) + dia_names()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("label" %in% names(ld))
  expect_true(nrow(ld) > 0)
})

test_that("dia_names returns one label per group per diagonal panel", {
  p <- ggcorrm(drosera) + dia_names()
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  # should have one row per group per diagonal panel
  panel_group_counts <- as.data.frame(table(ld$PANEL, ld$group))
  expect_true(all(panel_group_counts$Freq[panel_group_counts$Freq > 0] == 1))
})
