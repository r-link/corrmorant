context("Tests of stat_heatmap")

test_that("stat_heatmap builds without error", {
  p <- ggcorrm(drosera) + lotri(stat_heatmap(aes(fill = after_stat(corr))))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true("corr" %in% names(ld))
  expect_true(all(ld$corr >= -1 & ld$corr <= 1))
})

test_that("stat_heatmap returns one row per group per panel", {
  p <- ggcorrm(drosera) + lotri(stat_heatmap(aes(fill = after_stat(corr))))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  # each panel-group combination should have exactly one row
  panel_group_counts <- as.data.frame(table(ld$PANEL, ld$group))
  expect_true(all(panel_group_counts$Freq[panel_group_counts$Freq > 0] == 1))
})

test_that("stat_heatmap fills entire panel with Inf bounds", {
  p <- ggcorrm(drosera) + lotri(stat_heatmap(aes(fill = after_stat(corr))))
  pb <- ggplot_build(p)
  ld <- pb$data[[2]]
  expect_true(all(ld$xmin == -Inf))
  expect_true(all(ld$xmax == Inf))
  expect_true(all(ld$ymin == -Inf))
  expect_true(all(ld$ymax == Inf))
})
