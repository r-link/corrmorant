# StatHeatmap - ggproto object for stat_heatmap -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHeatmap <- ggproto("StatHeatmap", Stat,
                       required_aes = c("x", "y"),
                       compute_panel = function (self, data, scales,
                                                 corr_method, ...) {
                         dplyr::mutate(data,
                                       corr = stats::cor(x, y, use = "pairwise.complete.obs",
                                                         method = corr_method),
                                       x    = mean(scales$x$get_limits()),
                                       y    = mean(scales$y$get_limits()),
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
#'
#' @description `stat_heatmap()` is used to compute bivariate correlations and
#'   display them as correlation heat maps / point labels in the facets of
#'   [ggcorrm] plots.
#'
#' @inheritParams add_corrtext
#' @inheritParams ggplot2::layer
#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details  `stat_heatmap()` computes the correlation between variables in the
#'   facets of `ggcorrm` plots and returns the aesthetics necessary to either
#'   color the entire facet by correlation strength (`geom = "rect"`) or place a
#'   single point in the midpoint of each facet (`geom = "point"`).
#'
#' @rdname stat_heatmap
#'@seealso
#'   [ggplot2::layer()],
#'   [add_heatmap()]
#' @export
stat_heatmap <- function(mapping = NULL, data = NULL, geom = "rect",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE,
                         corr_method = NULL,
                         ...) {
  layer(
    stat = StatHeatmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(corr_method = corr_method, ...)
  )
}
