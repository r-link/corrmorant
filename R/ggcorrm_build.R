# ggplot_build method for ggcorrm objects -------------------------------------
#' @keywords internal
#' @export
ggplot_build.ggcorrm <- function(plot){
  # update layers to replace missing corrmorant parameters by the plot-level
  # values
  plot$layers <- lapply(plot$layers,
                        update_corrm_param,
                        plot_param = plot$plot_param)
  # use standard ggplot_build method (imported from ggplot2)
  ggplot_build.ggplot(plot)
}

# helper function for parameter update ----------------------------------------
#' @keywords internal
#' @importFrom dplyr intersect
update_corrm_param <- function(layer, plot_param){
  # test if parameters of corrmorant stats have to be updated
  update_param <- dplyr::intersect(names(layer$stat_params),
                                   names(plot_param))
  # replace parameters that are not yet set
  if (length(update_param) > 0) {
    for (i in update_param)
      layer$stat_params[[i]] <- layer$stat_params[[i]] %||% plot_param[[i]]
  }
  layer
}
