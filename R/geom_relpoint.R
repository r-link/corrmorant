#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRelpoint <- ggplot2::ggproto(
  "GeomRelpoint", GeomPoint,
  # define required aesthetics
  required_aes = c("relx", "rely"),
  # define panel plotting function
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    # get x and y ranges
    range <- coord$backtransform_range(panel_params)

    # convert relative to absolute ranges
    data$x <- range$x[1] + data$relx * diff(range$x)
    data$y <- range$y[1] + data$rely * diff(range$y)

    # pass results to regular draw_panel function for geom_text
    ggplot2::GeomPoint$draw_panel(data = data,
                                  panel_params = panel_params,
                                  coord = coord,
                                  na.rm = na.rm)
  }
)


#' @title add points scaled by correlation
#'
#' @description `geom_relpoint()` creates scatter plots based on relative
#'    coordinates.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams add_corrtext
#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details  `geom_relpoint()` behaves like  [ggplot2::geom_point()], but uses
#'   relative instead of absolute coordinates.
#'
#' @section Aesthetics:
#'   `geom_reltext()` requires the following aesthetics:
#'   \itemize{
#'   \item __relx__ (relative position on the x-axis)
#'   \item __rely__ (relative position on the y-axis)
#'   }
#'   In addition, it understands the same aesthetics as [ggplot2::geom_point()].
#'
#' @rdname geom_relpoint
#'@seealso
#'   [ggplot2::layer()],
#'   [ggplot2::geom_point()]
#' @export
geom_relpoint <- function(mapping = NULL, data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE){
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRelpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
