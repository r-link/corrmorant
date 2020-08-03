
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomReltext <- ggplot2::ggproto(
  "GeomText", Geom,
  # define required aesthetics
  required_aes = c("relx", "rely", "label"),
  # define default aesthetics
  default_aes = aes(
    colour = "black", size = 3.25, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),
  # define panel plotting function
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    # get x and y ranges
    range <- coord$backtransform_range(panel_params)

    # convert relative to absolute ranges
    data$x <- range$x[1] + data$relx * diff(range$x)
    data$y <- range$y[1] + data$rely * diff(range$y)

    # pass results to regular draw_panel function for geom_text
    ggplot2::GeomText$draw_panel(data = data,
                                 panel_params = panel_params,
                                 coord = coord,
                                 parse = parse,
                                 na.rm = na.rm,
                                 check_overlap = check_overlap)
  },
  # define legend keys
  draw_key = ggplot2::draw_key_text
)


#' @title Add text to relative positions
#'
#' @description `geom_reltext()` adds text labels to relative positions in a
#'   ggplot panel.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggcorrm

#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details `geom_reltext()` adds text to relative positions in ggplot panels,
#'   i.e. the position of the text is expressed as a number between 0 and 1
#'   (lower and upper axis limits) instead of a number on the scale of the
#'   displayed variables.
#'
#'   In corrmorant, it is widely used to display text labels relative to the
#'   panel margins instead of the data in the panel.
#'
#' @section Aesthetics:
#'   `geom_reltext()` requires the following aesthetics:
#'   \itemize{
#'   \item __relx__ (relative position on the x-axis)
#'   \item __rely__ (relative position on the y-axis)
#'   \item __label__
#'   }
#'   In addition, it understands the same aesthetics as [ggplot2::geom_text()]
#'
#' @seealso
#'  Used to position the output of [stat_corrtext()], [stat_corrtext()] and
#'  [stat_dia_names()].
#' @rdname geom_reltext
#' @export
geom_reltext <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomReltext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}
