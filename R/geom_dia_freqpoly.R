# GeomDiaFreqpoly -------------------------------------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDiaFreqpoly <-  ggplot2::ggproto(
  "GeomDiaFreqpoly", GeomPath,
  required_aes = "x",
  default_aes =
    aes(colour = "black", linetype = 1, alpha = NA, size = 0.5,
        lwr = 0.3, upr = 0.98),
  setup_data = Geom$setup_data,
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    # get x and y ranges
    range <- coord$backtransform_range(panel_params)

    # rescale range of frequency polygon
    data$y <- corrmorant:::rescale_var(data$ymax,
                                       lower = data$lwr[1],
                                       upper = data$upr[1],
                                       range = range$y,
                                       append_x = 0)

    # pass to regular draw_panel function
   GeomPath$draw_panel(data, panel_params, coord, arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      na.rm = na.rm)
  }
)

# geom_dia_freqpoly -----------------------------------------------------------
#' @rdname stat_dia_bin
#' @export
geom_dia_freqpoly <- function(mapping = NULL,
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
    geom = GeomDiaFreqpoly,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
