# add_heatmap -----------------------------------------------------------------
#' @title Correlation heatmaps in ggcorrm plots.
#'
#' @description `lotri_heatmap()` and `utri_heatmap()` are used to display
#'   correlation heatmaps in the lower/upper off-diagonal facets of [ggcorrm]
#'   plots. `lotri_heatpoint()` and `utri_heatpoint()` scale the and colour of a
#'   single centered [geom_point()][ggplot2::geom_point()] by correlation strength.
#'
#' @inheritParams add_corrtext
#' @inheritParams stat_heatmap
#' @inheritParams ggplot2::layer
#'
#' @param corr_size logical - should the `size` aesthetic of the points in
#'   `lotri/utri_heatpoint()` be expressed as a function of correlation
#'   strength? `corr_size = TRUE` Defaults to `TRUE`.
#' @param ... Additional arguments to [stat_heatmap()].
#' @return A `ggplot2` layer with correlation heatmaps / points indicating
#'   correlation strength.
#' @details `lotri_/utri_heatmap()` can be used to display correlation heat maps
#'   in the facets of `ggcorrm` plots.
#'
#'   `lotri/utri_heatmap()` are a wrapper around [stat_heatmap()] that
#'   additionally take care of the right specification of aesthetics.
#'
#'   `lotri/utri_heatpoint()` instead plots points positioned as in
#'   [stat_corrtext()] whose size on default depends on the strength of
#'   correlation. The shape parameter as well as transparency etc. can be
#'   adjusted via the `...` argument. The range of sizes covered by the
#'   'heatpoints' can be adjusted by [scale_size()][ggplot2::scale_size()].
#'
#' @seealso [stat_heatmap()], [stat_heatcircle()], [geom_relpoint()]
#' @name add_heatmap
NULL

# lotri_heatmap ---------------------------------------------------------------
#' @name add_heatmap
#' @export
lotri_heatmap <- function(corr_method = NULL, ...) {
  # return plot with labels
  lotri(
    stat_heatmap(mapping = aes(x = x, y = y, fill = ..corr..),
                 geom = "rect",
                 corr_method = corr_method,
                 inherit.aes = FALSE,
                 ...)
  )
}

# utri_heatmap ----------------------------------------------------------------
#' @name add_heatmap
#' @export
utri_heatmap <- function(corr_method = NULL, ...) {
  # return plot with labels
  utri(
    stat_heatmap(mapping = aes(x = x, y = y, fill = ..corr..),
                 geom = "rect",
                 corr_method = corr_method,
                 inherit.aes = FALSE,
                 ...)
  )
}

# lotri_heatpoint ---------------------------------------------------------------
#' @rdname add_heatmap
#' @export
lotri_heatpoint <- function(corr_size = TRUE, mapping = NULL, corr_method = "pearson", ...) {

  # update and check mapping
  mapping <- update_aes_corrm(mapping)
  if(corr_size) mapping <- modify_list(mapping, aes(size = ..corr..))

  # return layer
  lotri(
    geom_relpoint(mapping = mapping,
                   stat = "corrtext",
                   corr_method = corr_method,
                   show.legend = c(size = FALSE, colour = TRUE),
                   ...)
  )
}

# utri_heatpoint ---------------------------------------------------------------
#' @rdname add_heatmap
#' @export
utri_heatpoint <- function(corr_size = TRUE, mapping = NULL, corr_method = "pearson", ...) {

  # update and check mapping
  mapping <- update_aes_corrm(mapping)
  if(corr_size) mapping <- modify_list(mapping, aes(size = ..corr..))

  # return layer
  utri(
    geom_relpoint(mapping = mapping,
                   stat = "corrtext",
                   corr_method = corr_method,
                   show.legend = c(size = FALSE, colour = TRUE),
                   ...)
  )
}
