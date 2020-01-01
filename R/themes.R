# themes for ggcorrm plots ----------------------------------------------------

# standard theme for ggcorrm plots --------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param base_size PARAM_DESCRIPTION, Default: 9
#' @param base_family PARAM_DESCRIPTION, Default: ''
#' @param base_line_size PARAM_DESCRIPTION, Default: base_size/22
#' @param base_rect_size PARAM_DESCRIPTION, Default: base_size/22
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[grid]{unit}}
#' @rdname theme_corrm
#' @export 
#' @importFrom grid unit
theme_corrm <- function(base_size = 9, base_family = "", 
                        base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {
  # built upon theme_grey (standard ggplot theme)
  theme_grey(base_size, 
             base_family, 
             base_line_size, 
             base_rect_size) %+replace%
    theme(aspect.ratio = 1,
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = grid::unit(0, "mm"),
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          strip.background = element_blank()
          )
}
