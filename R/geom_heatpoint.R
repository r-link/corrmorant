# GeomHeatpoint - ggproto object for geom_heatpoint ---------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHeatpoint <- ggplot2::ggproto(
  "GeomHeatpoint", GeomPoint,
  required_aes = c("x", "y", "corr"),
  default_aes = aes(shape = 19, colour = "black", size = 1.5,
                     fill = NA, alpha = NA, stroke = 0.5),
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    # get final panel ranges
    range <- coord$backtransform_range(panel_params)

    # center point in panel
    data$x <- mean(range$x)
    data$y <- mean(range$y)

    ggplot2::GeomPoint$draw_panel(data = data,
                                  panel_params = panel_params,
                                  coord = coord,
                                  na.rm = na.rm)
  }
)


# geom_heatpoint() - geom function for GeomHeatpoint --------------------------
#' @title Display centered points with correlation-dependent aesthetics
#'
#' @description \code{geom_heatpoint()} displays a point centered in each panel
#'   using panel-aware positioning.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}()}.
#'
#' @return An object of class \code{Layer}.
#'
#' @details \code{geom_heatpoint()} uses \code{coord$backtransform_range()} to
#'   position a point at the center of each panel, ensuring correct positioning
#'   even when other layers modify the plot range.
#'
#' @seealso
#'   \code{\link{stat_heatmap}()},
#'   \code{\link[ggplot2]{geom_point}()}
#' @rdname geom_heatpoint
#' @export
geom_heatpoint <- function(mapping = NULL, data = NULL,
                           stat = "heatmap",
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHeatpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
