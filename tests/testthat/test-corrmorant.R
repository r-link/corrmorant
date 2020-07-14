context("Tests of corrmorant function")

test_that("corrmorant() and ggcorrm() create the same plots", {

  # prepare dataset for testing
  ldros <- dplyr::mutate_if(drosera, is.numeric, log)

  # style = "dark"
  p1 <- corrmorant(ldros, style = "dark")
  p2 <- ggcorrm(ldros, bg_dia = "grey20") +
    lotri(geom_point(alpha = min(1 / log10(nrow(ldros)), 1))) +
    utri_corrtext() +
    dia_density(lower = .3, fill = "grey90", col = 1)+
    dia_names(y_pos = .15, colour = "white", size = 3)

  p1$plot_env <- p2$plot_env <- NULL

  expect_equal(p1,p2)

  # style = "light
  p3 <- corrmorant(ldros, style = "light")
  p4 <- ggcorrm(ldros) +
    lotri(geom_point(alpha = min(1 / log10(nrow(ldros)), 1))) +
    utri_corrtext() +
    dia_density(lower = .3, fill = "grey80", col = 1)+
    dia_names(y_pos = .15, colour = "black", size = 3)

  p3$plot_env <- p4$plot_env <- NULL

  expect_equal(p3,p4)

  # style = "blue_red" (standard option)
  p5 <- corrmorant(ldros)
  p6 <- ggcorrm(ldros) +
    lotri(geom_point( aes(col = .corr), alpha = min(1 / log10(nrow(ldros)), 1))) +
    utri_corrtext(aes(col = .corr)) +
    dia_density(lower = .3, fill = "grey80", col = 1) +
    dia_names(y_pos = .15, colour = "black", size = 3) +
    scale_color_corr()

  p5$plot_env <- p6$plot_env <- NULL

  expect_equal(p5,p6)

})

