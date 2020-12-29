context("Tests of basic functionality of corrmorant layer types")

# create minimal base plot for plotting
p0 <- drosera %>%
  group_by(species) %>%
  dplyr::slice_head() %>%
  ungroup %>%
  ggcorrm()

# diagonal panel functions ----------------------------------------------------
test_that("dia_names() generates basic output", {
  expect_s3_class(p0 + dia_names(), c("ggcorrm", "gg", "ggplot"))
  }
  )

test_that("dia_density() generates basic output", {
  expect_s3_class(p0 + dia_density(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("dia_histogram() generates basic output", {
  expect_s3_class(p0 + dia_histogram(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("dia_freqpoly() generates basic output", {
  expect_s3_class(p0 + dia_histogram(), c("ggcorrm", "gg", "ggplot"))
}
)

# upper triangular panel functions --------------------------------------------
test_that("utri_corrtext() generates basic output", {
  expect_s3_class(p0 + utri_corrtext(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("utri_funtext() generates basic output", {
  expect_s3_class(p0 + utri_funtext(fun = mean), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("utri_heatcircle() generates basic output", {
  expect_s3_class(p0 + utri_heatcircle(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("utri_heatmap() generates basic output", {
  expect_s3_class(p0 + utri_heatmap(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("utri_heatpoint() generates basic output", {
  expect_s3_class(p0 + utri_heatpoint(), c("ggcorrm", "gg", "ggplot"))
}
)

# lower triangular panel functions --------------------------------------------
test_that("lotri_corrtext() generates basic output", {
  expect_s3_class(p0 + lotri_corrtext(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("lotri_funtext() generates basic output", {
  expect_s3_class(p0 + lotri_funtext(fun = mean), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("lotri_heatcircle() generates basic output", {
  expect_s3_class(p0 + lotri_heatcircle(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("lotri_heatmap() generates basic output", {
  expect_s3_class(p0 + lotri_heatmap(), c("ggcorrm", "gg", "ggplot"))
}
)

test_that("lotri_heatpoint() generates basic output", {
  expect_s3_class(p0 + lotri_heatpoint(), c("ggcorrm", "gg", "ggplot"))
}
)
