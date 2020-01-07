# StatHeatcircle - ggproto object for stat_heatcircle -------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHeatcircle <- ggproto("StatHeatcircle", Stat,
                       required_aes = c("x", "y"),
                       compute_panel = function (self, data, scales,
                                                 corr_method = "pearson",
                                                 rmin = 0.1, rmax = 0.9,
                                                 scale_by = "area", ...) {
                         # get power for scale
                         pow <- switch(scale_by, area = 0.5, radius = 1)

                         # test if minimum and maximum radius make sense
                         if (any(c(rmin, rmax) < 0 | c(rmin, rmax) > 1)) {
                           stop("rmin and rmax must be between 0 and 1.")
                         }
                         if (rmin > rmax) {
                           stop("rmin  larger than rmax.")
                         }

                         # get correlation
                         corr <- stats::cor(data$x, data$y,
                                            use = "pairwise.complete.obs",
                                            method = corr_method)
                         # get ranges
                         range_x <- range(data$x, na.rm = TRUE)
                         range_y <- range(data$y, na.rm = TRUE)
                         # prepare center coordinates
                         xc <- mean(range_x)
                         yc <- mean(range_y)
                         # get rescaled radii
                         rx  <- rescale_var(abs(corr) ^ pow,
                                           lower = 0,
                                           upper = diff(range_x)/ 2,
                                           range = c(rmin, rmax), append_x = c(0, 1))
                         ry  <- rescale_var(abs(corr) ^ pow,
                                            lower = 0,
                                            upper = diff(range_y)/ 2,
                                            range = c(rmin, rmax),
                                            append_x = c(0, 1))

                         # prepare coordinates for circle
                         x    <- xc + rx * cos(seq(0,  pi, length.out = 100))
                         ymax <- yc + ry * sin(seq(0,  pi, length.out = 100))
                         ymin <- yc + ry * sin(seq(0, -pi, length.out = 100))

                         # return everything
                         dplyr::filter(data, !duplicated(group)) %>%
                           dplyr::group_nest(group) %>%
                           dplyr::mutate(x = list(x),
                                         y = yc,
                                         ymin = list(ymin),
                                         ymax = list(ymax),
                                         corr = corr) %>%
                           tidyr::unnest(cols = c(x, ymin, ymax))
                       },
                       compute_group = function(self, data, scales, corr_method,
                                                rmin, rmax, scale_by, ...) data
)


# stat_heatcircle() - stat function based on heatcircle -----------------------------
#' @title Compute circles whose size represents correlation strength
#' @description \code{stat_heatcircle()} is used to compute bivariate correlations
#'     and display them in \code{\link{ggcorrm}} plots in form of circles whose color
#'     and size indicate correlation strength.
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created by
#'    \code{\link[ggplot2:aes]{aes()}}. \code{x} and \code{y} are set
#'    automatically and must not be changed,  but all other aesthetics
#'    may be manipulated. Defaults to \code{NULL} (use standard mapping).
#' @param corr_method character string with the correlation method passed
#'    to \code{\link[stats]{cor}}. Can be one of "pearson", "kendall" and
#'    "spearman". Defaults to "pearson" (or is inherited from the setting
#'    in \code{\link[ggcorrm]{ggcorrm()}}).
#' @param rmin numeric (0 - 1). Minimum radius for the "heat circles" (as a
#'    proportion of panel width). Defaults to 0.1.
#' @param rmax numeric (0 - 1). Maximum radius for the "heat circles" (as a
#'    proportion of panel width). Defaults to 0.9.
#' @param scale_by character.  \code{scale_by = "area"} scales the area of the
#'    circles by correlation strength, \code{scale_by = "radius"} scales by
#'    radius. Defaults to "area".
#' @param ... additional arguments passed to
#'     \code{\link[ggplot2:layer]{ggplot2::layer}}.
#' @return An object of class \code{Layer}.
#' @details  \code{stat_heatcircle()} computes the outlines of circles that
#'    are centered around the midpoint of \code{gcorrm} facets and whose radius
#'    or area is modified in dependence of correlation strength. \code{rmin} and
#'    \code{rmax} allow to modify the range in which the radii of the circles may
#'    vary. The circles are displayed using \code{\link[ggplot2]{geom_ribbon}}.
#' @rdname stat_heatcircle
#'@seealso
#'   \code{\link[ggplot2:layer]{ggplot2::layer}},
#'   \code{\link{geom_heatcircle}}
#' @export
stat_heatcircle <- function(mapping = NULL, data = NULL, geom = "ribbon",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE,
                         corr_method = "pearson", rmin = 0.1, rmax = 0.9,
                         scale_by = c("area", "radius"), ...) {
  # prepare scale argument
  scale_by <- arg_match(scale_by)
  layer(
    stat = StatHeatcircle, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(corr_method = corr_method, rmin = rmin, rmax = rmax,
                  scale_by = scale_by, ...)
  )
}

# geom_heatcircle - wrapper around stat_heatcircle ----------------------------------
#' @title Circles whose size represents correlation strength
#' @description \code{geom_heatcircle()} is used to display circles whose fill and
#'     size aesthetics depoend on the strength of bivariate correlations between
#'     variables. It is meant for the off-diagonal facets of \code{\link{ggcorrm}}
#'     plots.
#' @inheritParams stat_heatcircle
#' @param ... Additional arguments to \code{\link{stat_heatcircle}}.
#' @return A \code{ggplot2} layer with circles displaying correlation strength.
#' @details \code{geom_heatcircle()} is a wrapper around
#'    \code{\link[stat_heatcircle]{stat_heatcircle()}} that additionally takes
#'    care of the right specification of aesthetics.
#'
#'    \code{stat_heatcircle()} computes the outlines of circles that
#'    are centered around the midpoint of \code{gcorrm} facets and whose radius
#'    or area is modified in dependence of correlation strength. \code{rmin} and
#'    \code{rmax} allow to modify the range in which the radii of the circles may
#'    vary.
#'
#' @seealso
#'   \code{\link{stat_heatcircle}},
#'   \code{\link{geom_heatmap}}
#' @rdname geom_heatcircle
#' @export
geom_heatcircle <- function(corr_method = "pearson", rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"), ...) {
  # prepare scale argument
  scale_by <- arg_match(scale_by)

  # return plot with labels
  stat_heatcircle(mapping = aes(fill = ..corr..),
               geom = "ribbon",
               corr_method = corr_method,
               rmin = rmin, rmax = rmax,
               scale_by = scale_by, ...)
}

