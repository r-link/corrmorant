# StatDiaNames - ggproto object for the labelling function --------------------
# ggproto object for stat_dia_density
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaNames <- ggproto("StatDiaNames", Stat,
                        required_aes = c("x", "label"),
                        compute_group = function(data, scales, y_pos = 0.2) {
                          rx <- range(data$x, na.rm = TRUE)
                          data.frame(x = mean(rx),
                                     y = rx[1] + y_pos * diff(rx),
                                     label = data$label[1])
                        }
)

# stat_dia_names() - stat function based on StatDiaNames ----------------------
#' @title Positions for variable names in ggcorrm plots
#' @description \code{stat_dia_names()} is used by
#'      \code{\link[dia_names]{dia_names()}} to compute the positions of layer
#'      names in the diagonal panels of \code{\link{ggcorrm}} plots.
#' @param ... Additional arguments passed to \code{\link[ggplot2:layer]{layer()}}
#' @inheritParams ggplot2::layer
#' @inheritParams dia_names
#' @return An object of class \code{Layer}.
#' @details \code{stat_dia_names()} uses the range of variables on the diagonal
#'     facets of \code{\link{ggcorrm}} plots to compute appropriate positions
#'     for text labels of variable names.
#'@seealso
#'   \code{\link[ggplot2:layer]{ggplot2::layer}},
#'   \code{\link{dia_names}}
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
#' @description \code{dia_names()} is a wrapper around
#'    \code{\link[stat_dia_names]{stat_dia_names()}}
#'    which plots the names of variables in a \code{ggcorrm} plot at
#'    appropriate positions in the diagonal facets.
#' @param y_pos numeric between 0 and 1 specifying the relative position of the
#'     text labels along the x axis (defaults to 0.2).
#' @param mapping (optional) mapping for the internal call to
#'    \code{\link[stat_dia_names]{stat_dia_names()}}. Must be created with
#'    \code{\link[ggplot2:aes]{aes()}}, and must not contain \code{x} or
#'    \code{y}. Can e.g. be used to color text labels by groups (see example).
#'    Defaults to \code{NULL} (use standard mapping).
#' @param ... Additional parameters passed to
#'    \code{\link[stat_dia_names]{stat_dia_names()}}.
#' @return A \code{ggplot2} layer containing text labels on the diagonal facets.
#' @details \code{dia_names()} plots text labels on the plot diagonal and takes
#'    care of the often complicated positioning in plots with different x and y
#'    ranges.
#' @seealso
#'   \code{\link{stat_dia_names}},
#'   \code{\link{dia_density}},
#'   \code{\link{dia_histogram}},
#'   \code{\link{dia_freqpoly}}
#' @rdname dia_names
#' @export
dia_names <- function(y_pos = 0.2, mapping = NULL, ...) {
  if (any(c("x", "y", "label") %in% names(mapping))) {
    stop("x and y coordinates and labels in dia_names() may not be manipulated.")
  }
  # update mapping with standard aesthetics
  mapping <- modify_list(aes(x = x, label = var_x), mapping)
  # return plot with labels
  dia(stat_dia_names(mapping = mapping, geom = "text", y_pos = y_pos, ...))
}
