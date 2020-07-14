# StatDiaBin - ggproto object for stat_dia_bin() ------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaBin <- ggproto(
  "StatDiaBin", StatBin,
  default_aes = aes(x = x, y = after_stat(y)),
  # some of the warnings in StatBin are retained
  setup_params  = function(data, params){
    if (is.integer(data$x)) {
      stop("StatDiaBin requires a continuous x variable: the x variable is discrete.",
           call. = FALSE)
    }
    if (!is.null(params$boundary) && !is.null(params$center)) {
      stop("Only one of `boundary` and `center` may be specified.",
           call. = FALSE)
    }
    if (!is.null(params$width)) {
      stop("`width` is deprecated. Do you want `geom_bar()`?",
           call. = FALSE)
    }
    params
  },
  # rescaled output from StatDensity$compute_panel
  compute_panel = function (self, data, scales,
                            bins = bins, ...) {
    StatBin$compute_panel(data = data, scales = scales,
                          bins = bins, ...) %>%
      dplyr::mutate(
        ymin = rescale_var(0,
                           lower = 0,
                           upper = 1,
                           scales$y$get_limits(),
                           append_x = ncount),
        ymax = rescale_var(ncount,
                           lower = 0,
                           upper = 1,
                           scales$y$get_limits(),
                           append_x = 0),
        y = ymax
      ) %>%
      dplyr::select(-density, -count, -ncount, -ndensity)

  },
  # compute group: just called to make sure parameters() includes all important parameters
  compute_group = function (data, scales, binwidth = NULL, bins = NULL, center = NULL,
                            boundary = NULL, closed = c("right", "left"), pad = FALSE,
                            breaks = NULL, origin = NULL, right = NULL, drop = NULL,
                            width = NULL, lower = NULL, upper = NULL) {
    StatBin$compute_group(data, scales, binwidth, bins, center, boundary, closed, pad,
                          breaks, origin, right, drop, width)
  }
)


# stat_dia_bin() - stat function for StatDiaBin -------------------------------
#' @title Compute histograms and frequency polygons for ggcorrm plots.
#'
#' @description `stat_dia_bin()` computes the binned data summaries for the
#'   diagonal panels of [ggcorrm] plots that are created with [dia_histogram]
#'   and [dia_freqpoly].
#'
#' @inheritParams ggplot2::layer
#' @param lower numeric between 0 and 1. Lower limit of the histograms/frequency
#'   polygons relative to the range of the `y` axis. Defaults to 0.25.
#' @param upper numeric between 0 and 1. Upper limit of the histograms/frequency
#'   polygons relative to the range of the `y` axis. Defaults to 1.
#' @param ... Additional arguments passed [ggplot2::layer()] (arguments for
#'    [ggplot2::stat_bin()][ggplot2::geom_histogram()] are permitted).
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_dia_bin()` computes binned data summaries for display in the
#'   diagonal facets of `ggcorrm` plots. The `lower` and `upper` arguments can
#'   be used to offset the histograms/frequency polygons from zero and optimally
#'   fit them to the range of each panel.
#'
#' @seealso
#'     [ggplot2::stat_bin()][ggplot2::geom_histogram()],
#'    [dia_histogram()],
#'    [dia_freqpoly()]
#' @rdname stat_dia_bin
#' @export
stat_dia_bin <- function(mapping = NULL, data = NULL, geom = "dia_histogram",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, bins = 10,
                         ...) {
  layer(
    stat = StatDiaBin, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(bins = bins, ...)
  )
}
