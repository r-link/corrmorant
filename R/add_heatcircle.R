# add_heatcircle  -------------------------------------------------------------
#' @title Add circles whose size represents correlation strength
#' @description \code{lotri_heatcircle()} and \code{utri_heatcircle()} are used
#'   to display circles whose fill and  size aesthetics depend on correlation
#'   strength in the lower/upper off-diagonal facets of \code{\link{ggcorrm}}
#'   plots.
#' @inheritParams stat_heatcircle
#' @param ... Additional arguments to \code{\link{stat_heatcircle}}.
#' @return A \code{ggplot2} layer with circles displaying correlation strength.
#' @details \code{lotri_heatcircle()} and \code{utri_heatcircle()} are wrappers
#'   around \code{\link[stat_heatcircle]{stat_heatcircle()}} that additionally
#'   take care of the right specification of aesthetics.
#'
#'  \code{stat_heatcircle()} computes the outlines of circles that
#'  are centered around the midpoint of \code{gcorrm} facets and whose radius
#'  or area is modified in dependence of correlation strength. \code{rmin} and
#'  \code{rmax} allow to modify the range in which the radii of the circles may
#'  vary.
#'
#' @seealso
#'   \code{\link{stat_heatcircle}},
#'   \code{\link{stat_heatmap}}
#' @name add_heatcircle
NULL

# lotri_heatcircle  -----------------------------------------------------------
#' @rdname add_heatcircle
#' @export
lotri_heatcircle <- function(corr_method = "pearson", rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"), ...) {
  # prepare scale argument
  scale_by <- arg_match(scale_by)

  # return plot with labels
  lotri(
    stat_heatcircle(mapping = aes(fill = ..corr..),
                  geom = "ribbon",
                  corr_method = corr_method,
                  rmin = rmin, rmax = rmax,
                  scale_by = scale_by, ...)
  )
}


# utri_heatcircle  -----------------------------------------------------------
#' @rdname add_heatcircle
#' @export
utri_heatcircle <- function(corr_method = "pearson", rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"), ...) {
  # prepare scale argument
  scale_by <- arg_match(scale_by)

  # return plot with labels
  utri(
    stat_heatcircle(mapping = aes(fill = ..corr..),
                  geom = "ribbon",
                  corr_method = corr_method,
                  rmin = rmin, rmax = rmax,
                  scale_by = scale_by, ...)
  )
}
