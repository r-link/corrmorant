# StatHeatcircle - ggproto object for stat_heatcircle -------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHeatcircle <-  ggplot2::ggproto(
  "StatHeatcircle", Stat,
  required_aes = c("x", "y"),
  extra_params  = c("na.rm", "corr_method"),
  compute_panel = function (self, data, scales,
                            corr_method, ...) {
    # get correlation
    corr <- stats::cor(data$x, data$y,
                       use = "pairwise.complete.obs",
                       method = corr_method)

    # return one row per group with corr and center coordinates
    dplyr::filter(data, !duplicated(group)) %>%
      dplyr::mutate(
        corr = corr,
        x    = mean(scales$x$get_limits()),
        y    = mean(scales$y$get_limits())
      )
  },
  compute_group = function(self, data, scales, corr_method, ...) data
)


# stat_heatcircle() - stat function based on heatcircle -----------------------
#' @title Compute circles with correlation-dependent size
#'
#' @description `stat_heatcircle()` is used to compute bivariate correlations
#'   and display them in [ggcorrm] plots in form of circles whose color and size
#'   indicate correlation strength.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams add_corrtext
#' @param rmin numeric (0 - 1). Minimum radius for the "heat circles" (as a
#'   proportion of panel width). Defaults to 0.1.
#' @param rmax numeric (0 - 1). Maximum radius for the "heat circles" (as a
#'   proportion of panel width). Defaults to 0.9.
#' @param scale_by character.  `scale_by = "area"` scales the area of the
#'   circles by correlation strength, `scale_by = "radius"` scales by radius.
#'   Defaults to "area".
#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details  `stat_heatcircle()` computes the outlines of circles that are
#'   centered around the midpoint of `gcorrm` facets and whose radius or area is
#'   modified in dependence of correlation strength. `rmin` and `rmax` allow to
#'   modify the range in which the radii of the circles may vary. The circles
#'   are displayed using [ggplot2::geom_ribbon()].
#'
#' @rdname stat_heatcircle
#'@seealso
#'   [ggplot2::layer()],
#'   [add_heatcircle()]
#' @export
stat_heatcircle <- function(mapping = NULL, data = NULL, geom = GeomHeatcircle,
                            position = "identity", show.legend = NA,
                            inherit.aes = TRUE,
                            corr_method = NULL, rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"), ...) {
  # prepare scale argument
  scale_by <- rlang::arg_match(scale_by)
  ggplot2::layer(
    stat = StatHeatcircle, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(corr_method = corr_method, rmin = rmin, rmax = rmax,
                  scale_by = scale_by, ...)
  )
}

