# add_corrtext ----------------------------------------------------------------
#' @title Text labels for correlations in off-diagonal ggcorrm facets
#' @description \code{lotri_corrtext()} and \code{utri_corrtext()} are
#'   used to display text labels for the strength of bivariate correlations
#'   in the off-diagonal facets of \code{\link{ggcorrm}} plots.
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2:aes]{aes()}}. \code{x} and \code{y} are set
#'   automatically and must not be changed,  but all other aesthetics
#'   may be manipulated. Defaults to \code{NULL} (use standard mapping).
#' @param nrow integer - the number of rows for the correlation labels if
#'   grouping aesthetics are present (defaults to NULL - automatic setting
#'   of \code{nrow}).
#' @param ncol integer - the number of columns for the correlation labels if
#'   grouping aesthetics are present (defaults to NULL - automatic
#'   setting of \code{ncol}).
#' @param digits integer: Number of digits the correlations are rounded to
#'   (defaults to 2).
#' @param corr_size logical - should the \code{size} aesthetic be expressed
#'   as a function of correlation strength? \code{corr_size = TRUE} is a
#'   shorthand for setting \code{aes(size = abs(..corr..))}. Similar
#'   expressions can be used to access the correlation calculated by
#'   \code{stat_corrtext} manually in \code{aes()}. Defaults to
#'   \code{TRUE}.
#' @param corr_method character string with the correlation method passed
#'   to \code{\link[stats]{cor}}. Can be one of "pearson", "kendall" and
#'   "spearman". Defaults to "pearson" (or is inherited from the setting
#'   in \code{\link[ggcorrm]{ggcorrm()}}).
#' @param squeeze numeric between 0 an 1. Proportion of the facet
#'   width/height the facet labels are restricted to when multiple labels
#'   are present (defaults to 0.7 - labels extend over 70\% of the extent
#'   of the plot).
#' @param ... Additional arguments to \code{\link{stat_corrtext}}.
#' @return A \code{ggplot2} layer with text labels for correlation strength.
#' @details \code{lotri_corrtext()} and \code{utri_corrtext()} can be used
#'   to display the correlation  between variables in the lower or upper
#'   triangular facets of \code{ggcorrm} plots, respectively.
#'   Correlations can be calculated for single groups
#'   by using grouping aesthetics such as \code{aes(color = group)}.
#'
#'  code{lotri_corrtext()} and \code{utri_corrtext()} are wrappers around
#'  \code{\link[stat_corrtext]{stat_corrtext()}} that additionally take
#'  care of the right specification of aesthetics and allows to easily
#'  adjust size by correlation strength via \code{corr_size}.
#'
#' @seealso
#'   \code{\link[ggplot2:geom_text]{ggplot2::geom_text}},
#'   \code{\link{stat_corrtext}}
#' @name add_corrtext
NULL

# lotri_corrtext() ------------------------------------------------------------
#' @rdname add_corrtext
#' @export
lotri_corrtext <- function(mapping = NULL, nrow = NULL, ncol = NULL,
                           digits = 2, corr_size = TRUE,
                           corr_method = "pearson", squeeze = 0.7, ...) {
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in lotri_corrtext() may not be manipulated.")
  }

  # print warning if size is specified more than once
  if ("size" %in% names(mapping) && corr_size){
    warning("corr_size overridden by manually specified size.")
  }

  # add size by correlation manually if specified
  if(corr_size) mapping <- modify_list(aes(size = abs(..corr..)),
                                       mapping)

  # return plot with labels
  lotri(
    stat_corrtext(mapping = mapping, geom = "text", show.legend = FALSE,
                  ncol = ncol, nrow = nrow, corr_method = corr_method,
                  squeeze = squeeze, ...)
  )
}

# utri_corrtext() -------------------------------------------------------------
#' @rdname add_corrtext
#' @export
utri_corrtext <- function(mapping = NULL, nrow = NULL, ncol = NULL,
                          digits = 2, corr_size = TRUE,
                          corr_method = "pearson", squeeze = 0.7, ...) {
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in utri_corrtext() may not be manipulated.")
  }

  # print warning if size is specified more than once
  if ("size" %in% names(mapping) && corr_size){
    warning("corr_size overridden by manually specified size.")
  }

  # add size by correlation manually if specified
  if(corr_size) mapping <- modify_list(aes(size = abs(..corr..)),
                                       mapping)

  # return plot with labels
  utri(
    stat_corrtext(mapping = mapping, geom = "text", show.legend = FALSE,
                  ncol = ncol, nrow = nrow, corr_method = corr_method,
                  squeeze = squeeze, ...)
  )
}

