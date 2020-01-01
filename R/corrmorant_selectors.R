# corrmorant selector functions -----------------------------------------------
# corrmorant selectors (utri(), lotri() and dia()) must be able to manipulate
# the data displayed in each layer without interfering with any of the steps
# happening during the rendering process. While it would be possible to specify
# update function directly within the function call, I decided to use external
# modifiers instead to illustrate that what they are doing is outside regular
# ggplot syntax.
# Basically, the three selectors just take the (unevaluated) geom and make sure
# that when ggplot_build is called all calculations are performed only for the 
# data from the right subset of facets. To achieve this, the data slot in the
# corresponding geoms is replaced by a subsetting function. In cases where there
# are already (user-specified) data, these are filtered for the right facets, and
# in the unlikely case that there is a user-specified data function a new
# selector function is created that combines both selections.

# in addition, lotri(), utri() and dia() perform the necessary testing whether
# the data corrmorant functions are called upon are of the right class.

# there may be a better solution for all this, but I yet have to find it.


# layer_data_fun() function factory for layer_data functions ------------------
# returns a layer_data function that filters the data of a layer by the desired
# type (upper, lower, diag) 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param pos PARAM_DESCRIPTION
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
#'  \code{\link[methods]{is}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{join}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}}
#'  \code{\link[tidyr]{nest}}
#' @rdname update_data
#' @export 
#' @importFrom ggplot2 is.waive
#' @importFrom methods is
#' @importFrom dplyr filter select full_join mutate group_by ungroup
#' @importFrom tidyr unnest
update_data <- function(data, pos){
  # prepare function for subset computation if nothing is specified
  # (common case)
  if (ggplot2:::is.waive(data)) {
    datafun <- function(plot_data){ 
      if(!methods::is(plot_data, "tidy_corrm")){
        stop("corrmorant selectors can only be used in ggcorrm() calls\n")
      }
      dplyr::filter(plot_data, type == pos)
    }
  } else {
    # specify updated function if there is already a function for data computation
    # (unlikely to ever happen, but possible)
    if (is.function(data)){
      datafun <- function(plot_data){
        if(!methods::is(plot_data, "tidy_corrm")){
          stop("corrmorant selectors can only be used in ggcorrm() calls\n")
        }
        dplyr::filter(plot_data, type == pos) %>% data
      }
    } else {
      # if there are user-specified data:
      datafun <- function(plot_data){
        if(!methods::is(plot_data, "tidy_corrm")){
          stop("corrmorant selectors can only be used in ggcorrm() calls\n")
        }
        # if columns for facet identification are there return filtered dataset
        if (!any(!(c("var_x", "var_y", "type") %in% names(data)))){
          dplyr::filter(data, type == pos)
        } else {
          # get identifiers for panels
          panel_ids <- plot_data %>% 
            dplyr::select(var_x, var_y, type) %>% 
            dplyr::filter(!duplicated(paste(var_x, var_y)), 
                          type == pos)
          if (any((c("var_x", "var_y", "type") %in% names(data)))){
          # if some are present, merge with correct identifiers
          panel_ids %>% 
            dplyr::full_join(data)
          } else {
            # else combine with all levels
            dat <- replicate(nrow(panel_ids), data, simplify = FALSE)
            dplyr::mutate(panel_ids, dat = dat) %>% 
              dplyr::group_by(var_x, var_y, type) %>% 
              tidyr::unnest(cols = c(dat)) %>% 
              dplyr::ungroup()
          }
        }
      }
    }
  }
}

# lotri() - modify dataset of an existing geom  -------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param geom PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lotri
#' @export 
lotri <- function(geom) {
  geom$data <- update_data(geom$data, "lower")
  return(geom)
}

# utri() - modify dataset of an existing geom  --------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param geom PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname utri
#' @export 
utri <- function(geom) {
  geom$data <- update_data(geom$data, "upper")
  return(geom)
}

# dia() - modify dataset of an existing geom  ---------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param geom PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dia
#' @export 
dia <- function(geom) {
  geom$data <- update_data(geom$data, "diag")
  return(geom)
}
