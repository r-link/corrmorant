# StatHeatmap - ggproto object for stat_heatmap -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHeatmap <- ggproto("StatHeatmap", Stat,
                       required_aes = c("x", "y"),
                       compute_panel = function (self, data, scales,
                                                 corr_method = "pearson", ...) {
                         dplyr::mutate(data,
                                       corr = stats::cor(x, y, use = "pairwise.complete.obs",
                                                         method = corr_method),
                                       x    = mean(range(x, na.rm = TRUE)),
                                       y    = mean(range(y, na.rm = TRUE)),
                                       xmin = -Inf,
                                       xmax =  Inf,
                                       ymin = -Inf,
                                       ymax =  Inf) %>%
                           dplyr::filter(!duplicated(group))
                       },
                       compute_group = function(self, data, scales,
                                                corr_method, ...) data
)


# stat_heatmap() - stat function based on heatmap -----------------------------
#' @title Compute correlation heatmaps for ggcorrm plots
#' @description \code{stat_heatmap()} is used to compute bivariate correlations
#'     and display them as correlation heat maps / point labels in the facets
#'     of \code{\link{ggcorrm}} plots.
#' @inheritParams ggplot2::layer
#' @param mapping Set of aesthetic mappings created by
#'    \code{\link[ggplot2:aes]{aes()}}. \code{x} and \code{y} are set
#'    automatically and must not be changed,  but all other aesthetics
#'    may be manipulated. Defaults to \code{NULL} (use standard mapping).
#' @param corr_method character string with the correlation method passed
#'    to \code{\link[stats]{cor}}. Can be one of "pearson", "kendall" and
#'    "spearman". Defaults to "pearson" (or is inherited from the setting
#'    in \code{\link[ggcorrm]{ggcorrm()}}).
#' @param ... additional arguments passed to
#'     \code{\link[ggplot2:layer]{ggplot2::layer}}.
#' @return An object of class \code{Layer}.
#' @details  \code{stat_heatmap()} computes the correlation between variables
#'     in the facets of \code{ggcorrm} plots and returns the aesthetics
#'     necessary to either color the entire facet by correlation strength
#'     (\code{geom = "rect"}) or place a single point in the midpoint of each facet
#'     (\code{geom = "point"}).
#' @rdname stat_heatmap
#'@seealso
#'   \code{\link[ggplot2:layer]{ggplot2::layer}},
#'   \code{\link{geom_heatmap}}
#' @export
stat_heatmap <- function(mapping = NULL, data = NULL, geom = "rect",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE,
                         corr_method = "pearson",
                         ...) {
  layer(
    stat = StatHeatmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(corr_method = corr_method, ...)
  )
}

# geom_heatmap - wrapper around stat_heatmap ----------------------------------
#' @title Correlation heatmaps in ggcorrm plots.
#' @description \code{geom_heatmap()} is used to display correlation heatmaps
#'     in the off-diagonal facets of \code{\link{ggcorrm}} plots.
#'     \code{geom_heatpoint()} scales the and colour of a single centered
#'     \code{\link[ggplot2]{geom_point}} by correlation strength.
#' @inheritParams stat_heatmap
#' @param corr_size logical - should the \code{size} aesthetic of the
#'    points in `geom_heatpoint()` be expressed as a function of correlation
#'    strength? \code{corr_size = TRUE} Defaults to \code{TRUE}.
#' @param ... Additional arguments to \code{\link{stat_heatmap}}.
#' @return A \code{ggplot2} layer with correlation heatmaps / points indicating
#'    correlation strength.
#' @details \code{geom_heatmap()} can be used to display correlation heat maps
#'    in the facets of \code{ggcorrm} plots.
#'
#'    \code{geom_heatmap()} is a wrapper around
#'    \code{\link[stat_heatmap]{stat_heatmap()}} that additionally takes
#'    care of the right specification of aesthetics.
#'
#'    \code{geom_heatpoint()} instead plots a single point in the middle of the
#'    layer whose size on default depends on the strength of correlation. The
#'    shape parameter as well as transparency etc. can be adjusted via the
#'    \code{...} argument.
#'    The range of sizes covered by the 'heatpoints' can be adjusted by
#'    \code{\link[ggplot2:scale_size]{scale_size()}}.
#'
#'    For larger circles that scale with correlation, see
#'    \code{\link{geom_heatcircle()}}.
#'
#' @seealso
#'   \code{\link{stat_heatmap}},
#'   \code{\link{geom_heatcircle}}
#' @rdname geom_heatmap
#' @export
geom_heatmap <- function(corr_method = "pearson", ...) {
  # return plot with labels
  stat_heatmap(mapping = aes(fill = ..corr..),
               geom = "rect",
               corr_method = corr_method, ...)
}


# geom_heatpoint - wrapper around stat_heatmap ----------------------------------
#' @rdname geom_heatmap
#' @export
geom_heatpoint <- function(corr_size = TRUE, corr_method = "pearson", ...) {

  # define and update mapping
  mapping <- aes(color = ..corr..)
  if(corr_size) mapping <- modify_list(mapping, aes(size = abs(..corr..)))

  # return plot with labels
  stat_heatmap(mapping = mapping,
               geom = "point",
               corr_method = corr_method,
               show.legend = c(size = FALSE, colour = TRUE),
               ...)
}
