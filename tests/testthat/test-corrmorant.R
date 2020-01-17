context("Tests of corrmorant function")

test_that("corrmorant() and ggcorrm() create the same plots", {

  # regular ("dark") style
  p1 <- corrmorant(iris)
  p2 <- ggcorrm(iris, bg_dia = "grey20") +
    lotri(geom_point(alpha = min(1 / log10(nrow(iris)), 1))) +
    utri_corrtext() +
    dia_density(lower = .4, fill = "grey90", col = 1)+
    dia_names(y_pos = .1, colour = "white", size = 3)

  p1$plot_env <- p2$plot_env <- NULL

  expect_equal(p1,p2)

  # style = "light
  p3 <- corrmorant(iris, style = "light")
  p4 <- ggcorrm(iris) +
    lotri(geom_point(alpha = min(1 / log10(nrow(iris)), 1))) +
    utri_corrtext() +
    dia_density(lower = .4, fill = "grey80", col = 1)+
    dia_names(y_pos = .1, colour = "black", size = 3)

  p3$plot_env <- p4$plot_env <- NULL

  expect_equal(p3,p4)

  # style = "blue_red
  p5 <- corrmorant(iris, style = "blue_red")
  p6 <- ggcorrm(iris) +
    lotri(geom_point( aes(col = .corr), alpha = min(1 / log10(nrow(iris)), 1))) +
    utri_corrtext(aes(col = .corr)) +
    dia_density(lower = .4, fill = "grey80", col = 1) +
    dia_names(y_pos = .1, colour = "black", size = 3) +
    scale_color_corr(option = "A",
                     aesthetics = c("fill", "color"))

  p5$plot_env <- p6$plot_env <- NULL

  expect_equal(p5,p6)

})

