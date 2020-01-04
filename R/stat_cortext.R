# scripts for cortext and related functions -----------------------------------

# StatCortext - ggproto object for stat_Cortext -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCortext <- ggproto("StatCortext", Stat,
                       required_aes = c("x", "y"),
                       # compute panel - standard function just slightly updated to pass ranges
                       compute_panel = function (self, data, scales,
                                                 nrow = NULL, ncol = NULL,
                                                 digits = 2,
                                                 corr_method = "pearson",
                                                 squeeze = 0.7, ...) {
                         if (ggplot2:::empty(data)){
                           return(ggplot2:::new_data_frame())}
                         groups <- split(data, data$group)
                         stats <- lapply(groups, function(group) {
                           self$compute_group(data = group, scales = scales,
                                              corr_method = corr_method, ...)
                         })
                         stats <- mapply(function(new, old) {
                           if (ggplot2:::empty(new))
                             return(ggplot2:::new_data_frame())
                           unique <- ggplot2:::uniquecols(old)
                           missing <- !(names(unique) %in% names(new))
                           cbind(new, unique[rep(1, nrow(new)), missing,
                                             drop = FALSE])
                         }, stats, groups, SIMPLIFY = FALSE)

                         # bind rows
                         stats <- ggplot2:::rbind_dfs(stats)
                         # return output
                         get_cortext_pos(stats = stats,
                                         nrow = nrow, ncol = ncol,
                                         squeeze = squeeze,
                                         xrange = range(data$x, na.rm = TRUE),
                                         yrange = range(data$y, na.rm = TRUE))
                       },
                       # compute_group - modified from StatDensity
                       compute_group = function (data, scales,
                                                 nrow = NULL, ncol = NULL,
                                                 digits = 2,
                                                 corr_method = "pearson",
                                                 squeeze = 0.7, ...){
                         data.frame(corr = stats::cor(data$x, data$y,
                                               use = "pairwise.complete.obs",
                                               method = corr_method)) %>%
                           dplyr::mutate(label = format(x = corr, digits = digits))
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
                       }
)

# stat_cortext() - stat function based on CorText -----------------------------
#' @title Text labels for correlation strength in ggcorrm plots
#' @description \code{stat_dia_names()} is used to compute bivariate correlations
#'     and appropriate positions of text labels indicating correlation strength
#'     for the facets of \code{\link{ggcorrm}} plots.
#' @inheritParams geom_cortext
#' @inheritParams ggplot2::layer
#' @param ... additional arguments to \code{\link[ggplot2:layer]{ggplot2::layer}}.
#' @return An object of class \code{layer}.
#' @details  \code{stat_cortext()} computes the correlation between variables in
#'     the facets of \code{ggcorrm} plots and places text labels indicating the
#'     strength of correlation in appropriate positions within the facets.
#' @rdname stat_cortext
#'@seealso
#'   \code{\link[ggplot2:layer]{ggplot2::layer}},
#'   \code{\link{geom_cortext}}
#' @export
stat_cortext <- function(mapping = NULL, data = NULL, geom = "text",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, nrow = nrow, ncol = ncol,
                         digits = 2,
                         corr_method = "pearson", squeeze = 0.7,
                         ...) {
  layer(
    stat = StatCortext, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(digits = digits, corr_method = corr_method, squeeze = squeeze, ...)
  )
}

# geom_cortext - wrapper around stat_cortext ----------------------------------
#' @title Text labels for correlations in off-diagonal ggcorrm facets
#' @description FUNCTION_DESCRIPTION
#' @inheritParams ggplot2::layer
#' @param nrow PARAM_DESCRIPTION, Default: NULL
#' @param ncol PARAM_DESCRIPTION, Default: NULL
#' @param digits PARAM_DESCRIPTION, Default: 2
#' @param corrsize PARAM_DESCRIPTION, Default: TRUE
#' @param corr_method PARAM_DESCRIPTION, Default: 'pearson'
#' @param squeeze PARAM_DESCRIPTION, Default: 0.7
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
#' @rdname geom_cortext
#' @export
geom_cortext <- function(mapping = NULL, nrow = NULL, ncol = NULL,
                         digits = 2, corrsize = TRUE,
                         corr_method = "pearson", squeeze = 0.7, ...) {
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in geom_cortext() may not be manipulated.")
  }

    # add size by correlation manually if specified
  if(corrsize) mapping <- modify_list(aes(size = abs(..corr..)),
                                                mapping)

  # return plot with labels
  geom_text(mapping = mapping, stat = "cortext", show.legend = FALSE,
            ncol = ncol, nrow = nrow, corr_method = corr_method,
            squeeze = squeeze, ...)
}


# get_cortext_pos() function for cortext label positions ----------------------
#' @keywords internal
#' @importFrom dplyr mutate
get_cortext_pos <- function(stats, nrow = NULL, ncol = NULL, squeeze,
                            xrange, yrange){
  # return single group w/o computations if only one observation exists
  if(nrow(stats) <= 1) return(dplyr::mutate(stats, x = mean(xrange),
                                            y = mean(yrange)))
  else {
    # get number of groups
    ngr <- length(stats$group)
    # set dimensions of output (depending on which of nrow and ncol is set)
    if (is.null(nrow) & !is.null(ncol)){
      nrow <-  floor(ngr / ncol)
    } else {
      if (is.null(nrow)) nrow <- 1
      if (is.null(ncol)) ncol <- ceiling(ngr / nrow)
      # throw an error if both nrow and ncol exist and do not add up to the right number
      else if (ncol != ceiling(ngr / nrow)) stop("Check dimensions in geom_cortext: nrow and ncol values do not match")
    }
    # edit stats
    dplyr::mutate(stats,
           x = rescale_var(x = rep(1:ncol, length.out = ngr),
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = xrange),
           y = rescale_var(x = rep(nrow:1, each = ncol)[1:ngr],
                           lower = (1 - squeeze)/2,
                           upper = (1 + squeeze)/2,
                           range = yrange))
  }
}

