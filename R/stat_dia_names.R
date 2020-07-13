# StatDiaNames - ggproto object for the labelling function --------------------
# ggproto object for stat_dia_density
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaNames <- ggproto("StatDiaNames", Stat,
  required_aes = c("x", "label"),
  compute_panel = function(data, scales, y_pos = 0.2) {
    rx <- scales$x$get_limits()
    out <- data %>%
      dplyr::filter(!duplicated(group)) %>%
      dplyr::mutate(x = mean(rx),
                    y = mean(rx),
                    relx = 0.5,
                    rely = y_pos,
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
#' @param ... Additional arguments passed to [ggplot2::layer()].
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
#'  [ggplot2::layer()],
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
    params = list(y_pos = y_pos, ...)
  )
}
