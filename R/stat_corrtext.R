# this objects exists only to make use of ggproto inheritance rules
# to avoid dependencies from ggplot internals
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCorrtextProto <- ggplot2::ggproto(
  "StatCorrtextProto", Stat,
  required_aes = c("x", "y"),
  # compute_group
  compute_group = function (data, scales,
                            nrow = NULL, ncol = NULL,
                            digits = 2,
                            corr_method,
                            squeeze,
                            byrow = TRUE, ...){
    data.frame(corr = stats::cor(data$x, data$y,
                                 use = "pairwise.complete.obs",
                                 method = corr_method)) %>%
      dplyr::mutate(label = format(x = corr, digits = digits))
  }
)

# StatCorrtext - ggproto object for stat_Corrtext -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCorrtext <- ggplot2::ggproto(
  "StatCorrtext", StatCorrtextProto,
  # compute panel - standard function just slightly updated to pass ranges
  compute_panel = function (self, data, scales,
                            nrow = NULL, ncol = NULL,
                            digits = 2,
                            corr_method,
                            squeeze = 0.7,
                            byrow = TRUE, ...) {
    # compute stats with regular compute_panel function
    stats <- StatCorrtextProto$compute_panel(data = data,
                                             scales = scales,
                                             corr_method = corr_method,
                                             digits = digits,
                                             ...)
    # rescale output after computation
    get_corrtext_pos(stats = stats,
                     nrow = nrow, ncol = ncol,
                     squeeze = squeeze,
                     xrange = scales$x$get_limits(),
                     yrange = scales$y$get_limits(),
                     byrow = byrow)
  },
  setup_data = function(data, params){
    # check number of groups
    grouptab <- dplyr::group_by(data, PANEL) %>%
      dplyr::summarize(n = length(unique(group)))
    if (any(grouptab$n > 9)) {
      warning("Correlations calculated for very large number of groups per panel.\n",
              "Is this really what you want to do?")
    }
    data
  },
  # will not be evaluated, but argument names are needed:
  compute_group = function (data, scales,
                            nrow = NULL, ncol = NULL,
                            digits = 2,
                            corr_method,
                            squeeze,
                            byrow = TRUE, ...) {
    StatCorrtextProto$compute_group(data, scales, nrow, ncol, digits,
                                    corr_method, squeeze, ...)
  }
)

# stat_corrtext() - stat function based on Corrtext -----------------------------
#' @title Compute correlation strength for text labels in ggcorrm plots
#'
#' @description `stat_corrtext()` is used to compute bivariate correlations and
#'   appropriate positions of text labels indicating correlation strength for
#'   the facets of [ggcorrm] plots.
#'
#' @inheritParams add_corrtext
#' @inheritParams ggplot2::layer
#' @inheritParams ggcorrm
#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_corrtext()` computes the correlation between variables in the
#'   facets of `ggcorrm` plots and places text labels indicating the strength of
#'   correlation in appropriate positions within the facets.
#'
#' @rdname stat_corrtext
#'@seealso
#'  [lotri_corrtext()] and [utri_corrtext()]

#' @export
stat_corrtext <- function(mapping = NULL, data = NULL, geom = "text",
                          position = "identity", show.legend = NA,
                          inherit.aes = TRUE, nrow = NULL, ncol = NULL,
                          digits = 2,
                          corr_method = NULL, squeeze = 0.7,
                          byrow = TRUE,
                          ...) {
  ggplot2::layer(
    stat = StatCorrtext, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(digits = digits, corr_method = corr_method, squeeze = squeeze,
                  nrow = nrow, ncol = ncol, byrow = byrow, ...)
  )
}

# get_corrtext_pos() function for corrtext label positions ----------------------
#' @keywords internal
#' @importFrom dplyr mutate
get_corrtext_pos <- function(stats, nrow = NULL, ncol = NULL, squeeze,
                             xrange, yrange, byrow = TRUE){
  # return single group w/o computations if only one observation exists
  if(nrow(stats) <= 1) return(dplyr::mutate(stats,
                                            relx = 0.5,
                                            rely = 0.5,
                                            x = mean(xrange),
                                            y = mean(yrange)))
  else {
    # get number of groups
    ngr <- sum(!duplicated(stats$group))
    # set dimensions of output (depending on which of nrow and ncol is set)
    if (is.null(nrow) & is.null(ncol)) {
      nrow <-  round(sqrt(ngr))
      ncol <- ceiling(ngr / nrow)
    } else {
      if (is.null(nrow)) nrow <- ceiling(ngr / ncol)
      if (is.null(ncol)) ncol <- ceiling(ngr / nrow)
      # throw an error if both nrow and ncol exist and do not add up to the right number
      else if (ncol * nrow < ngr) stop("Check dimensions in text labels: nrow and ncol values do not match")
    }
    # compute coordinates
    # (both relative and absolute coordinates are computed. the absolute coordinates may cause problems
    #  in the presence of scale transformations or panel range extensions caused by other computed stats -
    #  in these cases better work with geom_reltext and relative coordinates)
    if (byrow){
      out <- dplyr::mutate(
        stats,
        relx = rescale_var(x = rep(1:ncol, length.out = ngr),
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = c(0, 1)),
        rely = rescale_var(x = rep(nrow:1, each = ncol)[1:ngr],
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = c(0, 1)),
        x = xrange[1] + .data$relx * diff(xrange),
        y = yrange[1] + .data$rely * diff(yrange)
      )
    } else {
      out <- dplyr::mutate(
        stats,
        relx = rescale_var(x = rep(1:ncol, each = nrow)[1:ngr],
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = c(0, 1)),
        rely = rescale_var(x = rep(nrow:1, length.out = ngr),
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = c(0, 1)),
        x = xrange[1] + .data$relx * diff(xrange),
        y = yrange[1] + .data$rely * diff(yrange)
      )
    }
    # return output
    out
  }
}


