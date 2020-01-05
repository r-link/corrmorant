#' @title corrmorant selectors
#' @description Selector functions that can be used to modify the mapping of
#'     \code{ggplot} \code{\link[ggplot2:layer]{layers}} to a subset of panels
#'     in \code{ggcorrm} plots.
#' @param layer A \code{ggplot} \code{\link[ggplot2]{layer}} (created by a call
#'      to a geom or a stat, or to the \code{\link[ggplot2]{layer}} function).
#' @details The corrmorant selector functions \code{lotri()}, \code{utri()}
#'     and \code{dia()} modify the data slot of \code{ggplot} layers
#'     (invoked by a call to a geom, a stat or
#'     \code{\link[ggplot2:layer]{layer()}}) to make sure that they
#'     are only displayed in the desired panels of \code{ggcorrm} plot.
#'
#'    \code{lotri()} shows the layer only in panels of the lower triangle,
#'     \code{utri()} in  the upper triangle and \code{dia()} in the panels
#'     of the plot diagonal.
#'
#'    If no data are specified explicitly in the layer, the selectors filter the
#'    data for the desired panels from the \code{\link{tidy_corrm}} data the
#'    plot is based upon. If data are specified in \code{layer} via its \code{data}
#'    argument, it either plots it to all upper/lower triangle or diagonal panels
#'    (depending on the type of selector) or matches it to the desired rows/columns
#'    if any variables named \code{var_x}, \code{var_y} and/or \code{type} are
#'    present in the new dataset.
#'
#'    The combination of \code{lotri()} and \code{utri()} in combination
#'    with \code{\link[geom_corrtext]{geom_corrtext()}} and regular ggplot2 geoms
#'    should be sufficient for the majority of use cases in the lower and upper
#'    triangle of a correlation plot.
#'    However, \code{dia()} in combination with regular
#'    geoms is often problematic for useful displays on the diagonal facets,
#'    as they are often reserved for data summaries that are difficult to place
#'    correctly in the facets when the data have different ranges.
#'    A series of common data summaries for
#'    the plot diagonal are provided with the functions \code{\link{dia_names}},
#'    \code{\link{dia_density}}, \code{\link{dia_histogram}} and
#'    \code{\link{dia_freqpoly}}, which automatically take care of the correct
#'    placement.
#'
#' @return A ggplot2 \code{\link[ggplot2]{layer}} with modified data.
#' @examples
#' \dontrun{
#' if(interactive()){
#' # plot with points on the lower triangle
#' ggcorrm(iris) +
#'   utri(geom_point(alpha = 0.4)) +
#'   lotri(geom_corrtext()) +
#'   dia_density(fill = "grey50") +
#'   dia_names(size = 3)
#'
#' # same plot with inverted lower and upper triangle
#' ggcorrm(iris) +
#'   lotri(geom_point(alpha = 0.4)) +
#'   utri(geom_corrtext()) +
#'   dia_density(fill = "grey50") +
#'   dia_names(size = 3)
#'  }
#' }
#' @seealso
#'   \code{\link{ggcorrm}},
#'   \code{\link{tidy_corrm}},
#'   \code{\link{corrmorant}},
#'   \code{\link{dia_names}},
#'   \code{\link{dia_density}},
#'   \code{\link{dia_histogram}},
#'   \code{\link{dia_freqpoly}}
#' @name corrmorant_selectors
NULL


# lotri() - modify dataset of an existing geom  -------------------------------
#' @rdname corrmorant_selectors
#' @export
lotri <- function(layer) {
  layer$data <- update_data(layer$data, "lower")
  return(layer)
}

# utri() - modify dataset of an existing geom  --------------------------------
#' @rdname corrmorant_selectors
#' @export
utri <- function(layer) {
  layer$data <- update_data(layer$data, "upper")
  return(layer)
}

# dia() - modify dataset of an existing geom  ---------------------------------
#' @rdname corrmorant_selectors
#' @export
dia <- function(layer) {
  layer$data <- update_data(layer$data, "diag")
  return(layer)
}

# update_data() - function factory for layer_data functions -------------------
# returns a layer_data function that filters the data of a layer by the desired
# type (upper, lower, diag)
#' @keywords internal
#' @importFrom methods is
#' @importFrom dplyr filter select full_join mutate group_by ungroup
#' @importFrom tidyr unnest
update_data <- function(data, pos){
  # prepare function for subset computation if nothing is specified
  # (regular case)
  if (is.waive(data)) {
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
