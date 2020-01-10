# StatDiaDensity - ggproto object for stat_dia_density ------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaDensity <- ggproto("StatDiaDensity", Stat,
                          required_aes = "x",
                          # compute panel - standard function just slightly updated to pass ranges
                          compute_panel = function (self, data, scales,
                                                    lower = .25, upper = 1, ...) {
                            if (ggplot2:::empty(data))
                              return(ggplot2:::new_data_frame())
                            groups <- split(data, data$group)
                            stats <- lapply(groups, function(group) {
                              self$compute_group(data = group, scales = scales, ...)
                            })
                            stats <- mapply(function(new, old) {
                              if (ggplot2:::empty(new))
                                return(ggplot2:::new_data_frame())
                              unique <- ggplot2:::uniquecols(old)
                              missing <- !(names(unique) %in% names(new))
                              cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
                            }, stats, groups, SIMPLIFY = FALSE)
                            # bind groups, rescale densities and return output
                            ggplot2:::rbind_dfs(stats) %>%
                              dplyr::mutate(y = rescale_var(density,
                                                            lower = lower,
                                                            upper = upper,
                                                            range = range(data$x, na.rm = TRUE),
                                                            append_x = 0))
                          },
                          # compute_group - modified from StatDensity
                          compute_group = function (data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                                                    n = 512, trim = FALSE, na.rm = FALSE, lower = lower, upper = upper)
                          {
                            if (trim) {
                              range <- range(data$x, na.rm = TRUE)
                            }
                            else {
                              range <- scales$x$dimension()
                            }
                            dens <- ggplot2:::compute_density(data$x, data$weight, from = range[1], to = range[2],
                                                              bw = bw, adjust = adjust, kernel = kernel, n = n)
                            # for correct display as polygons, end points have to be set manually when trimming
                            dens <- dplyr::bind_rows(data.frame(x = dens$x[1], density = 0),
                                                     dens,
                                                     data.frame(x = dens$x[nrow(dens)], density = 0))
                            # return density
                            return(dens)
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
# dia_density() - wrapper around stat_dia_density -----------------------------
#' @title Density curves for ggcorrm plots
#'
#' @description Add density curves to the diagonal panels of [ggcorrm] plots.
#'
#' @inheritParams ggcorrm
#' @inheritParams ggplot2::layer
#' @inheritParams stat_dia_bin
#' @param ... Additional parameters for [stat_dia_density()].
#'
#' @return A `ggplot2` layer with histograms or frequency polygons for the
#'   variables on the plot diagonal of `ggcorrm` plots.
#'
#' @details `dia_density()` adds density curves to the diagonal panels of
#'   `ggcorrm` plots. The placement of the curves is adjusted based on
#'   [stat_dia_density()]. The `lower` and`upper` arguments can be used to
#'   offset the density curves from zero and optimally fit them to the range of
#'   each panel.The standard values are chosen to work well when placing text
#'   labels under the histograms/frequency polygons with [dia_names].
#'
#' @seealso
#'   [ggplot2::geom_density()],
#'   [stat_dia_density()]
#' @rdname dia_density
#' @export
dia_density <- function(mapping = NULL, lower = .25, upper = 1, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping)

  # return plot with labels
  dia(geom_polygon(mapping = mapping, stat = "dia_density",
                   lower = lower, upper = upper, ...))
}

