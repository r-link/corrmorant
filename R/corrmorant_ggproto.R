#' @title ggproto classes for corrmorant
#' @description Class definitions for the new `ggproto` classes defined in
#'     the `corrmorant` package.
#' @section New stats:
#'
#' * `StatCorrtext`: create and position text labels for correlation strength
#'    (see [stat_corrtext] for details).
#' * `StatHeatmap`: create correlation heat maps (see [stat_heatmap]
#'    for details).
#' * `StatHeatcircle`: create circles indicating correlation strength (see
#'    [stat_heatcircle] for details).
#'* `StatDiaNames`: create and position text labels for variable names in the
#'    diagonal facets of `ggcorrm` plots (see [stat_dia_names] for details).
#' * `StatDiaDensity`: create and position density plots of variables in the
#'    diagonal facets of `ggcorrm` plots (see [stat_dia_density] for details).
#' * `StatDiaBin`: create and position plots of binned data (histograms or
#'   frequency polygons) of variables in the diagonal facets of `ggcorrm` plots
#'    (see [stat_dia_bin] for details).
#'
#' @seealso
#'  [ggplot2-ggproto],
#'  [lotri_corrtext] and [utri_corrtext],
#'  [lotri_heatmap] and [utri_heatmap],
#'  [lotri_heatcircle] and [utri_heatcircle],
#'  [dia_names],
#'  [dia_density],
#'  [dia_histogram]
#' @name corrmorant_ggproto
NULL
