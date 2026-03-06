# GeomHeatmap - ggproto object for geom_heatmap -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHeatmap <- ggplot2::ggproto(
  "GeomHeatmap", GeomRect,
  required_aes = c("x", "y", "corr"),
  default_aes = aes(fill = NA, colour = NA, linewidth = 0.1,
                     linetype = 1, alpha = NA),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    # get final panel ranges
    range <- coord$backtransform_range(panel_params)

    # set rect to fill entire panel
    data$xmin <- range$x[1]
    data$xmax <- range$x[2]
    data$ymin <- range$y[1]
    data$ymax <- range$y[2]

    ggplot2::GeomRect$draw_panel(data = data,
                                 panel_params = panel_params,
                                 coord = coord,
                                 linejoin = linejoin)
  },
  draw_key = ggplot2::draw_key_rect
)


# geom_heatmap() - geom function for GeomHeatmap ------------------------------
#' @title Display correlation heatmaps in ggcorrm plots
#'
#' @description \code{geom_heatmap()} displays correlation heatmaps using
#'   panel-aware positioning that correctly handles range changes from other
#'   layers.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}()}.
#'
#' @return An object of class \code{Layer}.
#'
#' @details \code{geom_heatmap()} is designed for use with
#'   \code{\link{stat_heatmap}()}. It uses the final panel range from
#'   \code{coord$backtransform_range()} to fill the entire panel, avoiding
#'   positioning errors when other layers modify the plot range.
#'
#' @seealso
#'   \code{\link{stat_heatmap}()},
#'   \code{\link[ggplot2]{geom_rect}()}
#' @rdname geom_heatmap
#' @export
geom_heatmap <- function(mapping = NULL, data = NULL,
                         stat = "heatmap",
                         position = "identity",
                         ...,
                         linejoin = "mitre",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHeatmap,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}
