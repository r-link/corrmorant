# dia_density() - function for density plots on the diagonal ------------------

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

# stat for dia_density() - corresponding stat function ------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mapping PARAM_DESCRIPTION, Default: NULL
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param geom PARAM_DESCRIPTION, Default: 'polygon'
#' @param position PARAM_DESCRIPTION, Default: 'identity'
#' @param show.legend PARAM_DESCRIPTION, Default: NA
#' @param inherit.aes PARAM_DESCRIPTION, Default: TRUE
#' @param lower PARAM_DESCRIPTION, Default: 0.25
#' @param upper PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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

# dia_density - wrapper around stat_dia_density -------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mapping PARAM_DESCRIPTION, Default: NULL
#' @param lower PARAM_DESCRIPTION, Default: 0.25
#' @param upper PARAM_DESCRIPTION, Default: 1
#' @param ... PARAM_DESCRIPTION
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
#' @rdname dia_density
#' @export
dia_density <- function(mapping = NULL, lower = .25, upper = 1, ...) {
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in dia_density() may not be manipulated.")
  }
  # update mapping with standard aesthetics
  mapping <- modify_list(aes(x = x, y = y), mapping)

  # return plot with labels
  dia(geom_polygon(mapping = mapping, stat = "dia_density",
                   lower = lower, upper = upper, ...))
}

