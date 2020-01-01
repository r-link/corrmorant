# functions for color and fill scales for correlations ------------------------

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

# scale_color_corr() - color scales based on .corr_scales() -------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param option PARAM_DESCRIPTION, Default: c("A", "B", "C", "D")
#' @param limits PARAM_DESCRIPTION, Default: c(-1, 1)
#' @param na.value PARAM_DESCRIPTION, Default: '#55FF55'
#' @param guide PARAM_DESCRIPTION, Default: 'colourbar'
#' @param aesthetics PARAM_DESCRIPTION, Default: 'colour'
#' @param direction PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname scale_color_corr
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param option PARAM_DESCRIPTION, Default: c("A", "B", "C", "D")
#' @param limits PARAM_DESCRIPTION, Default: c(-1, 1)
#' @param na.value PARAM_DESCRIPTION, Default: '#55FF55'
#' @param guide PARAM_DESCRIPTION, Default: 'colourbar'
#' @param aesthetics PARAM_DESCRIPTION, Default: 'fill'
#' @param direction PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname scale_fill_corr
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
