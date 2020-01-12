# StatDiaDensity - ggproto object for stat_dia_density ------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaDensity <- ggproto("StatDiaDensity", StatDensity,
  # rescaled output from StatDensity$compute_panel
  compute_panel = function (self, data, scales, lower, upper, ...) {
     StatDensity$compute_panel(data = data, scales = scales, ...) %>%
     split(.$group) %>%
     lapply(density_pad) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(y = rescale_var(density,
                                  lower = lower,
                                  upper = upper,
                                  range = scales$y$get_limits(),
                                  append_x = 0))
  },
  # ...just here because lower and upper have to be in the names of
  # compute_group to make compute_layer and parameters() work well
  compute_group = function(data, scales, bw = "nrd0", adjust = 1,
                           kernel = "gaussian", n = 512,
                           trim = FALSE, na.rm = FALSE,
                           lower = NULL, upper = NULL) {
    StatDensity$compute_group(data, scales, bw, adjust, kernel,
                              n, trim,  na.rm)
  }
)

# stat_dia_density() - stat function for StatDiaDensity -----------------------
#' @title Compute density curves for ggcorrm plots
#'
#' @description `stat_dia_density()` computes the density curves for the
#'   diagonal panels of [ggcorrm] plots that are created with [dia_density()].
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_dia_bin
#' @param ... Additional arguments passed to [ggplot2::layer()] (arguments for
#'   [ggplot2::stat_density()] are permitted).
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_density()` computes density curves for display in the diagonal
#'   facets of `ggcorrm` plots. The `lower` and `upper` arguments can be used to
#'   offset the density curves from zero and optimally fit them to the range of
#'   each panel.
#' @seealso
#'   [ggplot2::stat_density()],
#'   [dia_density]
#' @rdname stat_dia_density
#' @export
stat_dia_density <- function(mapping = NULL, data = NULL, geom = "polygon",
                             position = "identity", show.legend = NA,
                             inherit.aes = TRUE, lower = 0.25, upper = 1,
                             ...) {
  layer(
    stat = StatDiaDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(lower = lower, upper = upper, ...)
  )
}


# internal function for the padding of density values (needed for display shifted
# along the y axis)
#' @keywords internal
density_pad <- function(data){
  dplyr::bind_rows(dplyr::mutate(data[1, ], density = 0),
                   data,
                   dplyr::mutate(data[nrow(data), ], density = 0))
}
