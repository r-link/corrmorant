# corrmorant() - automated correlation matrix plots ---------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param style PARAM_DESCRIPTION, Default: c("dark", "light", "blue_red")
#' @param rescale PARAM_DESCRIPTION, Default: c("by_sd", "by_range", NULL)
#' @param labels PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname corrmorant
#' @export 
corrmorant <- function(data, style = c("dark", "light", "blue_red"), 
                       rescale     = c("by_sd", "by_range", NULL),
                       labels       = NULL,
                       ...){
  # match arguments
  rescale <- match.arg(rescale)
  style   <- match.arg(style)
  
  # prepare plot
  p0 <- ggcorrm(data, 
                rescale = rescale, 
                bg_dia =  switch(style, dark = "grey20", NULL), 
                labels = labels) 
  
  # prepare layers
  layers <- list(
  lotri(geom_point(alpha = min(1 / log10(nrow(data)), 1), 
                   mapping = switch(style, blue_red = aes(col = .corr), NULL))),
  utri(geom_cortext(mapping = switch(style, blue_red = aes(col = .corr), NULL))),
  dia_density(lower = .4, fill = switch(style, dark = "grey90", "grey80"), col = 1),
  dia_names(y_pos = .1, col = switch(style, dark = "white", 1), size = 3),
  switch(style, blue_red = scale_color_corr(option = "A",
                                            aesthetics = c("fill", "color")), NULL)
  )
  
  # return output
  p0 + layers
}

  
