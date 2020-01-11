# StatDiaBin - ggproto object for stat_dia_bin() ------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDiaBin <- ggproto("StatDiaBin", Stat,
                      required_aes = "x",
                      # compute panel - standard function just slightly updated to pass ranges
                      compute_panel = function (self, data, scales,
                                                lower = 0.25, upper = 1, barwidth = 0.9, ...) {
                        if (ggplot2:::empty(data))
                          return(ggplot2:::new_data_frame())
                        groups <- split(data, data$group)
                        stats <- lapply(groups, function(group) {
                          self$compute_group(data = group, scales = scales,
                                             lower = 0.25, upper = 1, barwidth = 0.9, ...)
                        })
                        stats <- mapply(function(new, old) {
                          if ( ggplot2:::empty(new))
                            return(ggplot2:::new_data_frame())
                          unique <- ggplot2:::uniquecols(old)
                          missing <- !(names(unique) %in% names(new))
                          cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
                        }, stats, groups, SIMPLIFY = FALSE)
                        # rescale computed stats to the diagonal panel size
                        ggplot2:::rbind_dfs(stats) %>%
                          dplyr::mutate(ymin = rescale_var(0, lower, upper,
                                                           scales$x$get_limits(),
                                                           append_x = ncount),
                                        ymax = rescale_var(ncount, lower, upper,
                                                           scales$x$get_limits(),
                                                           append_x = 0)
                                        )
                      },
                      # compute_group - slightly modified based on StatBin
                      compute_group =  function (data, scales, binwidth = NULL, bins = NULL, center = NULL,
                                                 boundary = NULL, closed = c("right", "left"), pad = FALSE,
                                                 breaks = NULL, origin = NULL, right = NULL, drop = NULL,
                                                 width = NULL, barwidth = 0.9, lower = 0.25, upper = 1)
                      {
                        if (!is.null(breaks)) {
                          if (!scales$x$is_discrete()) {
                            breaks <- scales$x$transform(breaks)
                          }
                          bins <- ggplot2:::bin_breaks(breaks, closed)
                        }
                        else if (!is.null(binwidth)) {
                          if (is.function(binwidth)) {
                            binwidth <- binwidth(data$x)
                          }
                          bins <- ggplot2:::bin_breaks_width(scales$x$dimension(), binwidth,
                                                             center = center, boundary = boundary, closed = closed)
                        }
                        else {
                          bins <- ggplot2:::bin_breaks_bins(scales$x$dimension(), bins, center = center,
                                                            boundary = boundary, closed = closed)
                        }
                        ggplot2:::bin_vector(data$x, bins, weight = data$weight, pad = pad) %>%
                          dplyr::mutate(xmin = x - barwidth * width / 2,
                                        xmax = x + barwidth * width / 2)
                      },
                      # setup_params function modified from StatBin
                      setup_params = function (data, params)
                      {
                        if (is.integer(data$x)) {
                          stop("StatDiaBin requires a continuous x variable.",
                               call. = FALSE)
                        }
                        if (!is.null(params$drop)) {
                          warning("`drop` is deprecated. Please use `pad` instead.",
                                  call. = FALSE)
                          params$drop <- NULL
                        }
                        if (!is.null(params$origin)) {
                          warning("`origin` is deprecated. Please use `boundary` instead.",
                                  call. = FALSE)
                          params$boundary <- params$origin
                          params$origin <- NULL
                        }
                        if (!is.null(params$right)) {
                          warning("`right` is deprecated. Please use `closed` instead.",
                                  call. = FALSE)
                          params$closed <- if (params$right)
                            "right"
                          else "left"
                          params$right <- NULL
                        }
                        if (!is.null(params$width)) {
                          stop("`width` is deprecated. In dia_histogram(), please use barwidth.",
                               call. = FALSE)
                        }
                        if (!is.null(params$boundary) && !is.null(params$center)) {
                          stop("Only one of `boundary` and `center` may be specified.",
                               call. = FALSE)
                        }
                        if (is.null(params$breaks) && is.null(params$binwidth) &&
                            is.null(params$bins)) {
                          ggplot2:::message_wrap("`stat_dia_bin()` using `bins = 10`. Pick better value with `binwidth`.")
                          params$bins <- 10
                        }
                        params
                      }
)

# stat_dia_bin() - stat function for StatDiaBin -------------------------------
#' @title Compute histograms and frequency polygons for ggcorrm plots.
#'
#' @description `stat_dia_bin()` computes the binned data summaries for the
#'   diagonal panels of [ggcorrm] plots that are created with [dia_histogram]
#'   and [dia_freqpoly].
#'
#' @inheritParams ggplot2::layer
#' @param lower numeric between 0 and 1. Lower limit of the histograms/frequency
#'   polygons relative to the range of the `y` axis. Defaults to 0.25.
#' @param upper numeric between 0 and 1. Upper limit of the histograms/frequency
#'   polygons relative to the range of the `y` axis. Defaults to 1.
#' @param barwidth Width of the histograms relative to the maximum possible
#'   width. Defaults to 0.9.
#' @param ... Additional arguments passed [ggplot2::layer()] (arguments for
#'   [ggplot2::stat_bin()] are permitted).
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_dia_bin()` computes binned data summaries for display in the
#'   diagonal facets of `ggcorrm` plots. The `lower` and `upper` arguments can
#'   be used to offset the histograms/frequency polygons from zero and optimally
#'   fit them to the range of each panel.
#'
#' @seealso
#'    [ggplot2:stat_bin][ggplot2::stat_bin],
#'    [dia_histogram],
#'    [dia_freqpoly]
#' @rdname stat_dia_bin
#' @export
stat_dia_bin <- function(mapping = NULL, data = NULL, geom = "rect",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, lower = 0.25, upper = 1,
                         barwidth = 0.9, ...) {
  layer(
    stat = StatDiaBin, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(lower = lower, upper = upper, barwidth = barwidth, ...)
  )
}
