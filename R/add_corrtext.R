# add_corrtext ----------------------------------------------------------------
#' @title Text labels for correlations in off-diagonal ggcorrm facets
#'
#' @description `lotri_corrtext()` and `utri_corrtext()` are used to display
#'   text labels for the strength of bivariate correlations in the off-diagonal
#'   facets of [ggcorrm] plots.
#'
#' @inheritParams ggcorrm
#' @param nrow integer - the number of rows for the correlation labels if
#'   grouping aesthetics are present (defaults to `NULL` - automatic setting of
#'   `nrow`).
#' @param ncol integer - the number of columns for the correlation labels if
#'   grouping aesthetics are present (defaults to `NULL` - automatic setting of
#'   `ncol`).
#' @param digits integer: Number of digits the correlations are rounded to
#'   (defaults to 2).
#' @param corr_size logical - should the `size` aesthetic be expressed as a
#'   function of correlation strength? `corr_size = TRUE` is a shorthand for
#'   setting `aes(size = abs(..corr..))`. Similar expressions can be used to
#'   access the correlation calculated by `stat_corrtext` manually in `aes()`.
#'   Defaults to `TRUE`.
#' @param corr_method character string with the correlation method passed to
#'   [cor()][stats]. Can be one of `"pearson"`, `"kendall"` and `"spearman"`.
#'   Defaults to `"pearson"` (or is inherited from the setting in [ggcorrm()]).
#' @param squeeze numeric between 0 an 1. Proportion of the facet width/height
#'   the facet labels are restricted to when multiple labels are present
#'   (defaults to 0.7 - labels extend over 70% of the extent of the plot).
#' @param ... Additional arguments to [stat_corrtext].
#' @return A `ggplot2` layer with text labels for correlation strength.
#' @details `lotri_corrtext()` and `utri_corrtext()` can be used to display the
#'   correlation  between variables in the lower or upper triangular facets of
#'   `ggcorrm` plots, respectively. Correlations can be calculated for single
#'   groups by using grouping aesthetics such as `aes(color = group)`.
#'
#'   `lotri_corrtext()` and `utri_corrtext()` are wrappers around
#'   [stat_corrtext()] that additionally take [stat_corrtext()] that
#'   additionally take care of the right specification of aesthetics and allows
#'   to easily adjust size by correlation strength via `corr_size`.
#'
#' @seealso [stat_corrtext]
#' @name add_corrtext
NULL

# lotri_corrtext() ------------------------------------------------------------
#' @rdname add_corrtext
#' @export
lotri_corrtext <- function(mapping = NULL, nrow = NULL, ncol = NULL,
                           digits = 2, corr_size = TRUE,
                           corr_method = "pearson", squeeze = 0.7, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping)

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
  # update and check mapping
  mapping <- update_aes_corrm(mapping)

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

