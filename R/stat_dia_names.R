# StatDiaNames - ggproto object for the labelling function --------------------
# ggproto object for stat_dia_density
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaNames <- ggproto("StatDiaNames", Stat,
                        required_aes = c("x", "label"),
                        compute_panel = function(data, scales, y_pos = 0.2) {
                          rx <- range(data$x, na.rm = TRUE)
                          out <- data %>%
                            filter(!duplicated(group)) %>%
                            mutate(x = rep(mean(rx)),
                                   y = rx[1] + y_pos * diff(rx),
                                   label = data$label[1])
                          if (nrow(out) > 1){
                            message("More than one group per panel detected in dia_names().\n",
                                    "Is this really what you want to do?")
                          }
                          return(out)
                          },
                        compute_group = function(data, scales, y_pos = 0.2) {
                          data
                        }
)

# stat_dia_names() - stat function based on StatDiaNames ----------------------
#' @title Positions for variable names in ggcorrm plots
#'
#' @description `stat_dia_names()` is used by [dia_names()] to compute the
#'   positions of layer names in the diagonal panels of [ggcorrm] plots.
#'
#' @param ... Additional arguments passed to[ggplot2:layer][layer()].
#' @inheritParams ggplot2::layer
#' @inheritParams dia_names
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_dia_names()` uses the range of variables on the diagonal
#'   facets of[ggcorrm] plots to compute appropriate positions for text labels
#'   of variable names.
#'
#'@seealso
#'  [ggplot2:layer][ggplot2::layer],
#'  [dia_names]
#' @rdname stat_dia_names
#' @export
stat_dia_names <- function(mapping = NULL, data = NULL, geom = "text",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, y_pos = 0.2,
                           ...) {
  layer(
    stat = StatDiaNames, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(y_pos = 0.2, ...)
  )
}

# dia_names() - label diagonal facets by variable names -----------------------
#' @title Add variable names to diagonal facets
#'
#' @description `dia_names()` is a wrapper around [stat_dia_names()] which plots
#'   the names of variables in a `ggcorrm` plot at appropriate positions in the
#'   diagonal facets.
#'
#' @inheritParams ggcorrm
#' @inheritParams ggplot2::layer
#' @param y_pos numeric between 0 and 1 specifying the relative position of the
#'   text labels along the x axis (defaults to 0.2).
#' @param ... Additional parameters passed to
#'   [stat_dia_names][stat_dia_names()].
#'
#' @return A `ggplot2` layer containing text labels on the diagonal facets.
#'
#' @details `dia_names()` plots text labels on the plot diagonal and takes care
#'   of the often complicated positioning in plots with different x and y
#'   ranges.
#'
#'   On default, `inherit.aes` is set to `FALSE` as in most cases this is the
#'   preferred behavior for the labels for variable names.
#'
#' @seealso
#'  [stat_dia_names]
#' @rdname dia_names
#' @export
dia_names <- function(y_pos = 0.2, mapping = NULL, ..., inherit.aes = FALSE) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, label = var_x))

  # return plot with labels
  dia(stat_dia_names(mapping = mapping, geom = "text", y_pos = y_pos,
                     inherit.aes = inherit.aes, ...))
}
