context("Tests of the ggcorrm_build function")

# create minimal base plot for plotting
p0 <- drosera %>%
  group_by(species) %>%
  dplyr::slice_head() %>%
  ungroup %>%
  ggcorrm(corr_method = "kendall",
          corr_group = species)

test_that("Global parameters passed correctly to stat_corrtext", {
  # corr_method passed on when no value is defined
  p1 <- p0 + stat_corrtext()
  ggplot_build(p1)
  expect_equal(
    p1$layers[[2]]$stat_params$corr_method,
    p0$plot_param$corr_method
  )

  # corr_method overridden when a value is given
  p2 <- p0 + stat_corrtext(corr_method = "spearman")
  ggplot_build(p2)
  expect_match(
    p2$layers[[2]]$stat_params$corr_method,
    "spearman"
  )
})


test_that("Global parameters passed correctly to stat_heatmap", {
  # corr_method passed on when no value is defined
  p1 <- p0 + stat_heatmap()
  ggplot_build(p1)
  expect_equal(
    p1$layers[[2]]$stat_params$corr_method,
    p0$plot_param$corr_method
  )

  # corr_method overridden when a value is given
  p2 <- p0 + stat_heatmap(corr_method = "spearman")
  ggplot_build(p2)
  expect_match(
    p2$layers[[2]]$stat_params$corr_method,
    "spearman"
  )
})


test_that("Global parameters passed correctly to stat_heatcircle", {
  # corr_method passed on when no value is defined
  p1 <- p0 + stat_heatcircle()
  ggplot_build(p1)
  expect_equal(
    p1$layers[[2]]$stat_params$corr_method,
    p0$plot_param$corr_method
  )

  # corr_method overridden when a value is given
  p2 <- p0 + stat_heatcircle(corr_method = "spearman")
  ggplot_build(p2)
  expect_match(
    p2$layers[[2]]$stat_params$corr_method,
    "spearman"
  )
})
