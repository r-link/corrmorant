# add_heatmap -----------------------------------------------------------------
#' @title Correlation heatmaps in ggcorrm plots.
#' @description `lotri_heatmap()` and `utri_heatmap()` are used to
#'   display correlation heatmaps in the lower/upper off-diagonal facets of
#'   [ggcorrm] plots.
#'   `lotri_heatpoint()` and `utri_heatpoint()` scale the and colour
#'   of a single centered [geom_point()][ggplot2] by correlation
#'   strength.
#' @inheritParams stat_heatmap
#' @param corr_size logical - should the `size` aesthetic of the
#'   points in `lotri/utri_heatpoint()` be expressed as a function of correlation
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
#'   `lotri/utri_heatpoint()` instead plots a single point in the middle of the
#'   layer whose size on default depends on the strength of correlation. The
#'   shape parameter as well as transparency etc. can be adjusted via the
#'   `...` argument.
#'   The range of sizes covered by the 'heatpoints' can be adjusted by
#'   [scale_size()][ggplot2:scale_size].
#'
#'   For larger circles that scale with correlation, see
#'   [add_heatcircle].
#'
#' @seealso
#'   Similar corrmorant stats: [stat_heatmap], [add_heatcircle]
#' @name add_heatmap
NULL

# lotri_heatmap ---------------------------------------------------------------
#' @name add_heatmap
#' @export
lotri_heatmap <- function(corr_method = "pearson", ...) {
  # return plot with labels
  lotri(
    stat_heatmap(mapping = aes(fill = ..corr..),
                 geom = "rect",
                 corr_method = corr_method, ...)
  )
}

# utri_heatmap ----------------------------------------------------------------
#' @name add_heatmap
#' @export
utri_heatmap <- function(corr_method = "pearson", ...) {
  # return plot with labels
  utri(
    stat_heatmap(mapping = aes(fill = ..corr..),
                 geom = "rect",
                 corr_method = corr_method, ...)
  )
}


# lotri_heatpoint ---------------------------------------------------------------
#' @rdname add_heatmap
#' @export
lotri_heatpoint <- function(corr_size = TRUE, corr_method = "pearson", ...) {

  # define and update mapping
  mapping <- aes(color = ..corr..)
  if(corr_size) mapping <- modify_list(mapping, aes(size = abs(..corr..)))

  # return plot with labels
  lotri(
    stat_heatmap(mapping = mapping,
                 geom = "point",
                 corr_method = corr_method,
                 show.legend = c(size = FALSE, colour = TRUE),
                 ...)
  )
}



# lotri_heatpoint ---------------------------------------------------------------
#' @rdname add_heatmap
#' @export
utri_heatpoint <- function(corr_size = TRUE, corr_method = "pearson", ...) {

  # define and update mapping
  mapping <- aes(color = ..corr..)
  if(corr_size) mapping <- modify_list(mapping, aes(size = abs(..corr..)))

  # return plot with labels
  utri(
    stat_heatmap(mapping = mapping,
                 geom = "point",
                 corr_method = corr_method,
                 show.legend = c(size = FALSE, colour = TRUE),
                 ...)
  )
}