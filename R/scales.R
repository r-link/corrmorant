# functions for color and fill scales for correlations ------------------------
#' @title Color and fill scales for correlation plots
#'
#' @description Diverging color gradients for the illustration of correlation
#'    strength.
#'
#' @param ... arguments passed to continuous scale
#' @param option character in c("A", "B", "C", "D") specifying the desired
#'    color scale. Defaults to "A".
#' @param limits Limits of the color gradient By default, the limits are set
#'    to c(-1, 1), and it is likely not a wise idea to change it as it might
#'    over-emphasise weak correlations.
#' @param na.value the color to use for missing values. Defaults to '#55FF55'.
#' @param guide Type of legend ("colourbar" for continuous legend, "legend"
#'    for legend with discrete values, or a `ggplot` guide function). Defaults
#'    to guide_colorbar(title = "Correlation") to have a reasonable standard
#'    scale title.
#' @param aesthetics Character string or vector of character strings with the
#'     aesthetics linked to the scale.  Defaults to  "color" for
#'     `scale_color_corr()` and "fill" for `scale_fill_corr()`.
#' @param direction Numeric indicating the order of the scale. Defaults to 1;
#'     negative values reverse the order of the scale.
#'
#' @details The color and fill scales specified via `option` are built upon
#'     [ggplot2::scale_colour_gradient2()] and [ggplot2::scale_fill_gradient2(),
#'     which can easily be used to set up additional scales if the available
#'     options are not sufficient.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' # drosera data with conditional coloring by correlation strength
#' ggcorrm(drosera) +
#'   lotri(geom_point(aes(col = .corr), alpha = 0.6)) +
#'   lotri(geom_smooth(aes(fill = .corr, col = .corr),
#'                     method = "lm", size = 0.3, alpha = 0.6)) +
#'   utri_corrtext(aes(color = .corr)) +
#'   dia_density(fill = "grey80", lower = .4) +
#'   dia_names(y_pos = .1) +
#'   scale_color_corr(option = "A", aesthetics = c("fill", "color"))
#'   }
#' }
#' @seealso
#'   * [ggplot2::scale_colour_gradient2()] - basic colour gradient scales
#'   * [ggplot2::scale_fill_gradient2()] - basic fill gradient scales
#' @name corrmorant_scales
NULL

# scale_color_corr() - color scales based on .corr_scales() -------------------
#' @rdname corrmorant_scales
#' @export
scale_color_corr <- function(...,
                             option = c("A", "B", "C", "D"),
                             limits = c(-1, 1),
                             na.value = "#55FF55",
                             guide = guide_colorbar(title = "Correlation"),
                             aesthetics = "colour",
                             direction = 1){
  # prepare option
  option <- rlang::arg_match(option)

  # choose desired color scheme
  values <- .corr_scales(option, direction)

  # return color scale
  scale_color_gradient2(...,
                        low = values[1],
                        mid = values[2],
                        high = values[3],
                        midpoint = 0,
                        limits = limits,
                        guide = guide,
                        aesthetics = aesthetics)
}

# alias for scale_color_corr
#' @rdname corrmorant_scales
#' @export
scale_colour_corr <- scale_color_corr

# scale_fill_corr() - color scales based on .corr_scales() --------------------
#' @rdname corrmorant_scales
#' @export
scale_fill_corr <- function(...,
                            option = c("A", "B", "C", "D"),
                            limits = c(-1, 1),
                            na.value = "#55FF55",
                            guide = guide_colorbar(title = "Correlation"),
                            aesthetics = "fill",
                            direction = 1) {
  # prepare option
  option <- rlang::arg_match(option)

  # choose desired color scheme
  values <- .corr_scales(option, direction)

  # return color scale
  scale_fill_gradient2(...,
                       low = values[1],
                       mid = values[2],
                       high = values[3],
                       midpoint = 0,
                       limits = limits,
                       guide = guide,
                       aesthetics = aesthetics)
}

# .corr_scales() - define scales for correlations  ----------------------------
# function that controls the choice of scales
# (negative values for direction turn around the order of colors)
.corr_scales <- function(option, direction = 1){
  values <- switch(option,
                   A = c("red", "grey70", "blue"),
                   B = c(scales::muted("red"), "grey70", scales::muted("blue")),
                   C = c(scales::muted("red"), "grey70", scales::muted("green2")),
                   D = c("#FDA512", "grey90", "#BE12DA")
  )
  # change order if desired
  if (direction < 0) values <- rev(values)
  # return output
  return(values)
}
