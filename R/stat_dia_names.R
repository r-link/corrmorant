# dia_names() - function for names on the diagonal ----------------------------

# ggproto object for stat_dia_density
StatDiaNames <- ggproto("StatDiaNames", Stat,
                        required_aes = c("x", "label"),
                        compute_group = function(data, scales, y_pos = 0.2) {
                          rx <- range(data$x, na.rm = TRUE)
                          data.frame(x = mean(rx),
                                     y = rx[1] + y_pos * diff(rx),
                                     label = data$label[1])
                        }
)

# stat for dia_density
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mapping PARAM_DESCRIPTION, Default: NULL
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param geom PARAM_DESCRIPTION, Default: 'text'
#' @param position PARAM_DESCRIPTION, Default: 'identity'
#' @param show.legend PARAM_DESCRIPTION, Default: NA
#' @param inherit.aes PARAM_DESCRIPTION, Default: TRUE
#' @param y_pos PARAM_DESCRIPTION, Default: 0.2
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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

# add_map has to be called with the add_aes function
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param y_pos PARAM_DESCRIPTION, Default: 0.2
#' @param mapping PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{character(0)}}
#' @rdname dia_names
#' @export
dia_names <- function(y_pos = 0.2, mapping = NULL, ...) {
  if (any(c("x", "y", "label") %in% names(mapping))) {
    stop("x and y coordinates and labels in geom_cortext() may not be manipulated.")
  }
  # update mapping with standard aesthetics
  mapping <- modify_list(aes(x = x, label = var_x), mapping)
  # return plot with labels
  dia(geom_text(mapping = mapping, stat = "dia_names", y_pos = y_pos, ...))
}
