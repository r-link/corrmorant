# add_heatmap -----------------------------------------------------------------
#' @title Correlation heatmaps in ggcorrm plots.
#' @description \code{lotri_heatmap()} and \code{utri_heatmap()} are used to
#'   display correlation heatmaps in the lower/upper off-diagonal facets of
#'   \code{\link{ggcorrm}} plots.
#'   \code{lotri_heatpoint()} and \code{utri_heatpoint()} scale the and colour
#'   of a single centered \code{\link[ggplot2]{geom_point}} by correlation
#'   strength.
#' @inheritParams stat_heatmap
#' @param corr_size logical - should the \code{size} aesthetic of the
#'   points in `lotri/utri_heatpoint()` be expressed as a function of correlation
#'   strength? \code{corr_size = TRUE} Defaults to \code{TRUE}.
#' @param ... Additional arguments to \code{\link{stat_heatmap}}.
#' @return A \code{ggplot2} layer with correlation heatmaps / points indicating
#'   correlation strength.
#' @details \code{lotri_/utri_heatmap()} can be used to display correlation heat maps
#'   in the facets of \code{ggcorrm} plots.
#'
#'   \code{lotri/utri_heatmap()} are a wrapper around
#'   \code{\link[stat_heatmap]{stat_heatmap()}} that additionally take
#'   care of the right specification of aesthetics.
#'
#'   \code{lotri/utri_heatpoint()} instead plots a single point in the middle of the
#'   layer whose size on default depends on the strength of correlation. The
#'   shape parameter as well as transparency etc. can be adjusted via the
#'   \code{...} argument.
#'   The range of sizes covered by the 'heatpoints' can be adjusted by
#'   \code{\link[ggplot2:scale_size]{scale_size()}}.
#'
#'   For larger circles that scale with correlation, see
#'   \code{\link{add_heatcircle}}.
#'
#' @seealso
#'   \code{\link{stat_heatmap}},
#'   \code{\link{add_heatcircle}}
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
