# GeomDiaHistogram  -----------------------------------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDiaHistogram <- ggplot2::ggproto(
  "GeomDiaHistogram", GeomBar,
  required_aes = "x",
  default_aes =
    aes(fill = "grey80", colour = NA,
        linetype = 1, alpha = NA, size = 0.3,
        lwr = 0.3, upr = 0.98),
  setup_data = Geom$setup_data,
  draw_panel = function(self, data, panel_params, coord, width = NULL, ...)
  {
    # get x and y ranges
    range <- coord$backtransform_range(panel_params)

    # rescale range of histogram
    data$ymax <- corrmorant:::rescale_var(data$ymax,
                                          lower = data$lwr[1],
                                          upper = data$upr[1],
                                          range = range$y,
                                          append_x = 0)
    data$ymin <- range$y[1] + 0.99 * data$lwr[1] * diff(range$y)

    # return grob
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params,
                                              coord)
  }
)

# geom_dia_histogram  -----------------------------------------------------------
#' @rdname stat_dia_bin
#' @export
geom_dia_histogram <- function(mapping = NULL,
                               data = NULL,
                               stat = "dia_bin",
                               position = "identity",
                               ...,
                               na.rm = TRUE,
                               show.legend = NA,
                               inherit.aes = FALSE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDiaHistogram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
