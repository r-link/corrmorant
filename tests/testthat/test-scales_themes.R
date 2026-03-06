context("Tests of corrmorant scales and themes")

# access internal function
corr_scales <- corrmorant:::.corr_scales

test_that(".corr_scales returns correct colour vectors", {
  # All options return 3-element vectors
  for (opt in c("A", "B", "C", "D")) {
    cols <- corr_scales(opt)
    expect_length(cols, 3)
  }
  # Option A is red-grey-blue
  expect_equal(corr_scales("A"), c("red", "grey70", "blue"))
})

test_that(".corr_scales respects direction", {
  cols_fwd <- corr_scales("A", direction = 1)
  cols_rev <- corr_scales("A", direction = -1)
  expect_equal(cols_fwd, rev(cols_rev))
})

test_that("scale_color_corr creates a valid scale", {
  s <- scale_color_corr(option = "A")
  expect_s3_class(s, "Scale")
  # Check it's a continuous scale
  expect_true(s$is_discrete() == FALSE)
})

test_that("scale_fill_corr creates a valid scale", {
  s <- scale_fill_corr(option = "B")
  expect_s3_class(s, "Scale")
})

test_that("scale_colour_corr is an alias for scale_color_corr", {
  expect_identical(scale_colour_corr, scale_color_corr)
})

test_that("theme_corrm returns a valid theme", {
  th <- theme_corrm()
  expect_s3_class(th, "theme")
  # aspect ratio should be 1
  expect_equal(th$aspect.ratio, 1)
  # panel background should be blank
  expect_s3_class(th$panel.background, "element_blank")
  # panel grid should be blank
  expect_s3_class(th$panel.grid.major, "element_blank")
  expect_s3_class(th$panel.grid.minor, "element_blank")
})

test_that("theme_corrm respects base_size", {
  th <- theme_corrm(base_size = 14)
  expect_s3_class(th, "theme")
})
