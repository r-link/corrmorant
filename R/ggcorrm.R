# ggcorm() - the workhorse function -------------------------------------------
# define ggcorm function 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param labels PARAM_DESCRIPTION, Default: NULL
#' @param rescale PARAM_DESCRIPTION, Default: c("by_sd", "by_max", NULL)
#' @param corr_group PARAM_DESCRIPTION, Default: NULL
#' @param corr_method PARAM_DESCRIPTION, Default: 'pearson'
#' @param mutates PARAM_DESCRIPTION, Default: NULL
#' @param bg_dia PARAM_DESCRIPTION, Default: NULL
#' @param bg_lotri PARAM_DESCRIPTION, Default: NULL
#' @param bg_utri PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{reexports}}
#' @rdname ggcorrm
#' @export 
#' @importFrom dplyr tibble
ggcorrm <- function(data,          # dataset
                    labels = NULL, # replacement labels for facet names
                    rescale = c("by_sd", "by_max", NULL),  # rescaling options
                    corr_group = NULL,
                    # settings for data-level correlations
                    corr_method = "pearson",
                    mutates = NULL,    # post-reshaping mutates
                    # background options
                    bg_dia = NULL, 
                    bg_lotri = NULL, 
                    bg_utri = NULL){ 
  # if rescale argument is not changed, pick first
  rescale <- match.arg(rescale)
  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!is_quosures(mutates)) 
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }
  
  # catch grouping variable
  corr_group <- enquo(corr_group)
  
  # prepare data
  corrdat <- tidy_corrm(data, 
                        labels      = labels, 
                        rescale     = rescale, 
                        corr_group  = corr_group,
                        corr_method = corr_method, 
                        mutates     = mutates)

  # prepare plot
  plot_out <- ggplot(data = corrdat, mapping = aes(x = x, y = y),
                     corr_method = corr_method, corr_use = corr_use) +
    facet_grid(var_x ~ var_y, scales = "free") +
    geom_blank() + # add geom_blank to set dimensions
    theme_corrm()
  
  # add background layer if desired
  if(any(!is.null(list(bg_dia, bg_lotri, bg_utri)))){
    bgdat <- dplyr::tibble(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    bgs <- vector(mode = "list", length = 3)
    if(!is.null(bg_dia)){
      bgs[[1]] <- dia(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                                data = bgdat, fill = bg_dia, inherit.aes = FALSE))
    }
    if(!is.null(bg_lotri)){
      bgs[[2]] <- lotri(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                                  data = bgdat, fill = bg_lotri, inherit.aes = FALSE))
    }
    if(!is.null(bg_utri)){
      bgs[[3]] <- utri(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                                 data = bgdat, fill = bg_utri, inherit.aes = FALSE))
    }
    plot_out <- plot_out + bgs
  }
  
  # return plot with updated class
  return(structure(plot_out,
                   class = c(class(plot_out), "ggcorrm")))
}
