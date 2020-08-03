# dia_names() - label diagonal facets by variable names -----------------------
#' @title Add variable names to diagonal facets
#'
#' @description `dia_names()` is a wrapper around [stat_dia_names()] which plots
#'   the names of variables in a `ggcorrm` plot at appropriate positions in the
#'   diagonal facets.
#'
#' @inheritParams add_corrtext
#' @inheritParams ggcorrm
#' @inheritParams ggplot2::layer
#' @param y_pos numeric between 0 and 1 specifying the relative position of the
#'   text labels along the x axis (defaults to 0.2).
#' @param ... Additional parameters passed to [stat_dia_names()].
#'
#' @return A `ggplot2` layer containing text labels on the diagonal facets.
#'
#' @details `dia_names()` plots text labels on the plot diagonal and takes care
#'   of the often complicated positioning in plots with different x and y
#'   ranges.
#'
#'   On default, `inherit.aes` is set to `FALSE` as in most cases this is the
#'   preferred behavior for the labels for variable names.
#'
#' @seealso
#'  [stat_dia_names()]
#' @rdname dia_names
#' @export
dia_names <- function(y_pos = 0.15, mapping = NULL, ..., inherit.aes = FALSE) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, label = var_x))

  # return plot with labels
  dia(stat_dia_names(mapping = mapping, geom = "reltext", y_pos = y_pos,
                     inherit.aes = inherit.aes, ...))
}

# dia_histogram() - wrapper around stat_dia_bin -------------------------------
#' @title Histograms and frequency polygons for ggcorrm plots
#'
#' @description Add histograms or frequency polygons to the diagonal panels of
#'   [ggcorrm] plots
#'
#' @inheritParams add_corrtext
#' @inheritParams stat_dia_bin
#' @inheritParams ggcorrm
#' @inheritParams ggplot2::layer
#'
#' @param ... Additional parameters for [stat_dia_bin()].
#'
#' @return A `ggplot2` layer with histograms or frequency polygons for the
#'   variables on the plot diagonal of `ggcorrm` plots.
#'
#' @details  The `lower` and`upper` arguments can be used to offset the
#'   histograms/frequency polygons from zero and optimally fit them to the range
#'   of each panel. The standard values are chosen to work well when placing
#'   text labels under the histograms/frequency polygons with [dia_names()].
#'
#'   `dia_histogram()` adds histograms of the numeric variables in a `ggcorrm`
#'   plot to the plot diagonal.
#'   Frequency polygons can be created with `dia_freqpoly()`. Both functions use
#'   the same stat, [stat_dia_bin()], which is built upon
#'   [ggplot2::stat_bin()][ggplot2::geom_histogram()].
#'
#' @seealso
#'   [ggplot2::stat_bin()][ggplot2::geom_histogram()],
#'   [stat_dia_bin()]
#' @rdname dia_histogram
#' @export
dia_histogram <- function(mapping = NULL, lower = .3, upper = 0.98,
                          bins = 10, position = "identity", ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, lwr = lower, upr = upper))

  # return plot with labels
  dia(
    geom_dia_histogram(
      mapping = mapping,
      stat = "dia_bin",
      position = position,
      bins = bins,
      ...)
  )
}

# dia_freqpoly() - wrapper around stat_dia_bin --------------------------------
#' @rdname dia_histogram
#' @export
dia_freqpoly <- function(mapping = NULL, lower = .3, upper = 0.98,
                         bins = 15, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, lwr = lower, upr = upper))

  # return plot with labels
  dia(geom_dia_freqpoly(mapping = mapping, stat = "dia_bin",
                        pad = TRUE, bins = bins, ...))
}

# dia_density() - wrapper around stat_dia_density -----------------------------
#' @title Density curves for ggcorrm plots
#'
#' @description Add density curves to the diagonal panels of [ggcorrm] plots.
#'
#' @inheritParams add_corrtext
#' @inheritParams ggcorrm
#' @inheritParams ggplot2::layer
#' @inheritParams stat_dia_bin
#' @param ... Additional parameters for [geom_dia_density()].
#'
#' @return A `ggplot2` layer with histograms or frequency polygons for the
#'   variables on the plot diagonal of `ggcorrm` plots.
#'
#' @details `dia_density()` adds density curves to the diagonal panels of
#'   `ggcorrm` plots. The placement of the curves is adjusted based on
#'   [geom_dia_density()]. The `lower` and`upper` arguments can be used to
#'   offset the density curves from zero and optimally fit them to the range of
#'   each panel.The standard values are chosen to work well when placing text
#'   labels under the histograms/frequency polygons with [dia_names].
#'
#' @seealso
#'   [ggplot2::geom_density()],
#'   [geom_dia_density()]
#' @rdname dia_density
#' @export
dia_density <- function(mapping = NULL, lower = .3, upper = 0.98, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping, standard_aes = aes(x = x,
                                                          lwr = lower,
                                                          upr = upper))

  # return plot with labels
  dia(
    geom_dia_density(
      mapping = mapping, ...)
    )
}


