# GeomDiaDensity - ggproto object for geom_dia_density ------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDiaDensity <-  ggplot2::ggproto(
  "GeomDiaDensity", GeomDensity,
  required_aes = c("x"),
  default_aes =
    aes(fill = "grey80", weight = 1, colour = "black",
        linetype = 1, alpha = NA, size = 0.3,
        lwr = 0.3, upr = 0.98),
  setup_data = Geom$setup_data,
  draw_panel = function(self, data, panel_params, coord, ...) {
    # get x and y ranges
    range <- coord$backtransform_range(panel_params)

    # rescale range of density values
    data$ymax <- corrmorant:::rescale_var(data$density,
                                          lower = data$lwr[1],
                                          upper = data$upr[1],
                                          range = range$y,
                                          append_x = 0)
    data$ymin <- range$y[1] + 0.99 * data$lwr[1] * diff(range$y)

    # compute group-level grobs
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ...)
    })

    # name grobs
    grob <-  grid::gTree(children = do.call("gList", grobs))
    grob$name <- grid::grobName(grob, "dia_density")

    # return grobs
    grob
  }
)


# geom_dia_density() - geom function for GeomDiaDensity -----------------------
#' @title Add density curves to the plot diagonal of ggcorrm plots
#'
#' @description `geom_dia_density()` rescales the output of
#'   [ggplot2::stat_density()][ggplot2::geom_density()] for the correct display
#'   of density curves on the plot diagonal of [ggcorrm] plots.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_dia_bin
#' @inheritParams ggplot2::geom_density
#' @param ... Additional arguments passed to [ggplot2::layer()] (arguments for
#'   [ggplot2::geom_density()] are permitted).
#'
#' @return An object of class `Layer`.
#'
#' @details `geom_density()` computes density curves for display in the diagonal
#'   facets of `ggcorrm` plots. Its `lwr` and `upr` aesthetics can be used to
#'   offset the density curves from zero and optimally fit them to the range of
#'   each panel.
#' @seealso
#'   [ggplot2::geom_density()],
#'   [dia_density()]
#' @rdname geom_dia_density
#' @export
geom_dia_density <- function(mapping = NULL,
                          data = NULL,
                          stat = "density",
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          outline.type = "full") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDiaDensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      outline.type = outline.type,
      ...
    )
  )
}
