#' @title corrmorant selectors
#'
#' @description Selector functions that can be used to modify the mapping of
#'   `ggplot` [layers][ggplot2::layer] to a subset of panels in `ggcorrm` plots.
#'
#' @param layer A `ggplot` layer (created by a call to a geom or a stat, or to
#'   the [ggplot2::layer()]).
#'
#' @details The corrmorant selector functions `lotri()`, `utri()` and `dia()`
#'   modify the data slot of `ggplot` layers (invoked by a call to a geom, a
#'   stat or [layer()][ggplot2::layer]) to make sure that they are only displayed
#'   in the desired panels of `ggcorrm` plot.
#'
#'   `lotri()` shows the layer only in panels of the lower triangle, `utri()` in
#'   the upper triangle and `dia()` in the panels of the plot diagonal.
#'
#'   If no data are specified explicitly in the layer, the selectors filter the
#'   data for the desired panels from the [tidy_corrm] data the plot is based
#'   upon. If data are specified in `layer` via its `data` argument, it either
#'   plots it to all upper/lower triangle or diagonal panels (depending on the
#'   type of selector) or matches it to the desired rows/columns if any
#'   variables named `var_x`, `var_y` and/or `pos` are present in the new
#'   dataset.
#'
#'   The combination of `lotri()` and `utri()` in combination with  regular
#'   ggplot2 geoms should be sufficient for the a large number of use cases for
#'   data displa in the lower and upper triangle of a correlation plot. However,
#'   `dia()` in combination with regular geoms is often problematic for useful
#'   displays on the diagonal facets, as they are often reserved for data
#'   summaries that are difficult to place correctly in the facets when the data
#'   have different ranges. A series of common data summaries for the plot
#'   diagonal are provided with the functions [dia_names], [dia_density],
#'   [dia_histogram] and [dia_freqpoly], which automatically take care of the
#'   correct placement.
#'
#'   In addition, there are plenty of stats specifically designed for the use in
#'   offdiagonal corrmorant facets that all exist in two versions, e.g.
#'   [lotri_corrtext] and  [utri_corrtext].
#'
#' @return A ggplot2 [layer][ggplot2::layer()] with modified data.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' # plot with points on the lower triangle
#' ggcorrm(iris) +
#'   utri(geom_point(alpha = 0.4)) +
#'   lotri_corrtext() +
#'   dia_density(fill = "grey50") +
#'   dia_names(size = 3)
#'
#' # same plot with inverted lower and upper triangle
#' ggcorrm(iris) +
#'   lotri(geom_point(alpha = 0.4)) +
#'   utri_corrtext() +
#'   dia_density(fill = "grey50") +
#'   dia_names(size = 3)
#'  }
#' }
#' @seealso
#'  [ggcorrm()],
#'  [tidy_corrm()],
#'  [corrmorant()]
#' @name corrmorant_selectors
NULL


# lotri() - modify dataset of an existing geom  -------------------------------
#' @rdname corrmorant_selectors
#' @export
lotri <- function(layer) {
  if (!inherits(layer, "Layer")) {
    stop("lotri() has to be called on ggplot layers.")
  }
  layer$data <- update_data(layer$data, "lotri")
  return(layer)
}

# utri() - modify dataset of an existing geom  --------------------------------
#' @rdname corrmorant_selectors
#' @export
utri <- function(layer) {
  if (!inherits(layer, "Layer")) {
    stop("utri() has to be called on ggplot layers.")
  }
  layer$data <- update_data(layer$data, "utri")
  return(layer)
}

# dia() - modify dataset of an existing geom  ---------------------------------
#' @rdname corrmorant_selectors
#' @export
dia <- function(layer) {
  if (!inherits(layer, "Layer")) {
    stop("dia() has to be called on ggplot layers.")
  }
  layer$data <- update_data(layer$data, "dia")
  return(layer)
}

# update_data() - function factory for layer_data functions -------------------
# returns a layer_data function that filters the data of a layer by the desired
# position (utri, lotri, dia)
#' @keywords internal
#' @importFrom methods is
#' @importFrom dplyr filter select full_join mutate group_by ungroup
#' @importFrom tidyr unnest
update_data <- function(data, target){
  # prepare function for subset computation if nothing is specified
  # (regular case)
  if (is.waive(data)) {
    datafun <- function(plot_data){
      if(!methods::is(plot_data, "tidy_corrm")){
        stop("corrmorant selectors can only be used in ggcorrm() calls\n")
      }
      dplyr::filter(plot_data, pos == target)
    }
  } else {
    # specify updated function if there is already a function for data computation
    # (unlikely to ever happen, but possible)
    if (is.function(data)){
      datafun <- function(plot_data){
        if(!methods::is(plot_data, "tidy_corrm")){
          stop("corrmorant selectors can only be used in ggcorrm() calls\n")
        }
        dplyr::filter(plot_data, pos == target) %>%
          data()
      }
    } else {
      # test if specified data are in a valid format
      if (!inherits(data, "data.frame")) {
        stop("corrmorant selector called on a layer with unrecognized data format.\nShould be a data.frame or function.")
      }
      # if there are user-specified data:
      datafun <- function(plot_data){
        if(!methods::is(plot_data, "tidy_corrm")){
          stop("corrmorant selectors can only be used in ggcorrm() calls\n")
        }
        # get names of tidy_corrm columns in the layer dataset
        tc_cols <- intersect(colnames(data), c("var_x", "var_y", "pos"))

        # get panel_ids in the plot dataset
        panel_ids <- plot_data %>%
          dplyr::select(var_x, var_y, pos) %>%
          dplyr::filter(!duplicated(paste(var_x, var_y)),
                        pos == target)

        # if no tidycorrm columns are specified, return updated input
        if (length(tc_cols) == 0) {
          output <- dplyr::mutate(data, pos = target) %>%
            dplyr::left_join(panel_ids)
          return(output)
        } else {
          # test if all specified columns are valid
          # (i.e. there are no levels that do not occur in the data)
          test <- mapply(function(x, y) any(!(unique(x) %in% unique(y))),
                         data[, tc_cols],
                         plot_data[, tc_cols])
          if(any(test)){
            stop("Layer data contain variable names missing in the correlation matrix.\n")
          } else {
            # return filtered data if "pos" is present
            if ("pos" %in% tc_cols){
              output <- dplyr::filter(data, pos == target) %>%
                dplyr::left_join(panel_ids)
              return(output)
            } else {
              # else return updated raw data
              output <- dplyr::mutate(data, pos = target) %>%
                dplyr::left_join(panel_ids)
              return(output)
            }
          }
        }


        # # if columns for facet identification are there return filtered dataset
        # if (!any(!(c("var_x", "var_y", "pos") %in% names(data)))){
        #   dplyr::filter(data, pos == target)
        # } else {
        #   # get identifiers for panels
        #   panel_ids <- plot_data %>%
        #     dplyr::select(var_x, var_y, pos) %>%
        #     dplyr::filter(!duplicated(paste(var_x, var_y)),
        #                   pos == target)
        #   if (any((c("var_x", "var_y", "pos") %in% names(data)))){
        #     # if some are present, merge with correct identifiers
        #     panel_ids %>%
        #       dplyr::full_join(data)
        #   } else {
        #     # else combine with all levels
        #     dat <- replicate(nrow(panel_ids), data, simplify = FALSE)
        #     dplyr::mutate(panel_ids, dat = dat) %>%
        #       dplyr::group_by(var_x, var_y, pos) %>%
        #       tidyr::unnest(cols = c(dat)) %>%
        #       dplyr::ungroup()
        #   }
        # }
      }
    }
  }
}
