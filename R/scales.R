# functions for color and fill scales for correlations ------------------------

#' @title Color and fill scales for correlation plots
#' @description Diverging color gradients for the illustration of correlation
#'    strength.
#' @param ... arguments passed to continuous scale
#' @param option character in c("A", "B", "C", "D") specifying the desired
#'    color scale. Defaults to "A".
#' @param limits Limits of the color gradient By default, the limits are set
#'    to c(-1, 1), and it is likely not a wise idea to change it as it might
#'    over-emphasise weak correlation
#' @param na.value the color to use for missing values. Defaults to '#55FF55'.
#' @param guide Type of legend ("colourbar" for continuous legend, "legend"
#'    for legend with discrete values). Defaults to "colourbar".
#' @param aesthetics character string or vector of character strings with the
#'     aesthetics linked to the scale.  Defaults to  "color" for
#'     \code{scale_color_corr()} and "fill" for \code{scale_fill_corr()}.
#' @param direction numeric indicating the order of the scale. Defaults to 1;
#'     negative values reverse the order of the scale.
#' @details The color and fill scales specified via \code{option} are built upon
#'     \code{\link[ggplot2:scale_colour_gradient]{ggplot2::scale_color_gradient1}},
#'     which can easily be used to set up additional scales if the available
#'     options are not sufficient.
#' @examples
#' \dontrun{
#' if(interactive()){
#' # iris data with conditional coloring by correlation strength
#' ggcorrm(iris, rescale = "by_sd") +
#'   lotri(geom_point(aes(col = .corr), alpha = 0.6)) +
#'   lotri(geom_smooth(aes(fill = .corr, col = .corr),
#'                     method = "lm", size = 0.3, alpha = 0.6)) +
#'   utri(geom_cortext(aes(color = .corr))) +
#'   dia_density(fill = "grey80", lower = .4) +
#'   dia_names(y_pos = .1) +
#'   scale_color_corr(option = "A", aesthetics = c("fill", "color"))
#'   }
#' }
#' @seealso
#'   \code{\link[ggplot2:scale_colour_gradient]{ggplot2::scale_color_gradient1}},
#'   \code{\link{ggcorrm}},
#'   \code{\link{tidy_corrm}},
#'   \code{\link{corrmorant}}
#' @name corrmorant_scales
NULL

# scale_color_corr() - color scales based on .corr_scales() -------------------
#' @rdname corrmorant_scales
#' @export
scale_color_corr <- function(...,
                             option = c("A", "B", "C", "D"),
                             limits = c(-1, 1),
                             na.value = "#55FF55",
                             guide = "colourbar",
                             aesthetics = "colour",
                             direction = 1){
  # prepare option
  option <- match.arg(option)

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


# scale_fill_corr() - color scales based on .corr_scales() --------------------
#' @rdname corrmorant_scales
#' @export
scale_fill_corr <- function(..., option = c("A", "B", "C", "D"),
                            limits = c(-1, 1),
                            na.value = "#55FF55",
                            guide = "colourbar",
                            aesthetics = "fill",
                            direction = 1) {
  # prepare option
  option <- match.arg(option)

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
