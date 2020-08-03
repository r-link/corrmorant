# add_heatcircle  -------------------------------------------------------------
#' @title Add circles with correlation-dependent size
#'
#' @description `lotri_heatcircle()` and `utri_heatcircle()` are used to display
#'   circles whose fill and size aesthetics depend on correlation strength in
#'   the lower/upper off-diagonal facets of [ggcorrm] plots.
#'
#' @inheritParams add_corrtext
#' @inheritParams stat_heatcircle
#' @inheritParams ggplot2::layer
#'
#' @param ... Additional arguments to [stat_heatcircle].
#'
#' @return A `ggplot2` layer with circles displaying correlation strength.
#'
#' @details `lotri_heatcircle()` and `utri_heatcircle()` are wrappers around
#'   [stat_heatcircle()] that additionally take care of the right specification
#'   of aesthetics.
#'
#'   `stat_heatcircle()` computes the outlines of circles that are centered
#'   around the midpoint of `ggcorrm` facets and whose radius or area is
#'   modified in dependence of correlation strength. `rmin` and `rmax` allow to
#'   modify the range in which the radii of the circles may vary. By standard,
#'   the `fill` aesthetic is mapped to `.corr`, but this can be overridden by
#'   specifying a different mapping.
#'
#' @seealso
#'   [stat_heatcircle],
#'   [stat_heatmap]
#' @name add_heatcircle
NULL

# lotri_heatcircle  -----------------------------------------------------------
#' @rdname add_heatcircle
#' @export
lotri_heatcircle <- function(mapping = NULL,
                             corr_method = NULL,
                             rmin = 0.1, rmax = 0.9,
                             scale_by = c("area", "radius"), ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, y = y,
                                                 fill = ..corr..)
                              )

  # prepare scale argument
  scale_by <- rlang::arg_match(scale_by)

  # return plot with labels
  lotri(
    stat_heatcircle(mapping = mapping,
                    geom = "ribbon",
                    corr_method = corr_method,
                    rmin = rmin, rmax = rmax,
                    scale_by = scale_by, ...)
  )
}


# utri_heatcircle  -----------------------------------------------------------
#' @rdname add_heatcircle
#' @export
utri_heatcircle <- function(mapping = NULL,
                            corr_method = NULL,
                            rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"), ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, y = y,
                                                 fill = ..corr..)
                              )

  # prepare scale argument
  scale_by <- rlang::arg_match(scale_by)

  # return plot with labels
  utri(
    stat_heatcircle(mapping = mapping,
                    geom = "ribbon",
                    corr_method = corr_method,
                    rmin = rmin, rmax = rmax,
                    scale_by = scale_by, ...)
  )
}
