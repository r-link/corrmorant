# add_funtext ----------------------------------------------------------------
#' @title User-specified text labels in off-diagonal ggcorrm facets
#'
#' @description `lotri_funtext()` and `utri_funtext()` are used to display
#'   user-specified text labels in the off-diagonal facets of [ggcorrm] plots.
#'
#' @inheritParams add_corrtext
#' @inheritParams ggcorrm
#' @param mapping Set of aesthetic mappings created by [aes()][ggplot2::aes()]. x
#'   and y are set automatically and must not bechanged, but all other
#'   aesthetics may be manipulated. By default, the `fill` aesthetic is mapped
#'   to `..fun_out..` internally, but this is overridden when explicitly
#'   specified. Defaults to `NULL` (use standard settings).
#' @param ... Additional arguments to [stat_funtext].
#'
#' @return A `ggplot2` layer with text labels for user_specified functions.
#'
#' @details `lotri_funtext()` and `utri_funtext()` can be used to display user-
#'   defined text labels in the lower or upper triangular facets of
#'   `ggcorrm` plots, respectively. The functions can be evaluated separately
#'   for different groups by using grouping aesthetics such as `aes(color = group)`.
#'
#'   If `fun` is a function or rlang lambda-style [one-sided formula][rlang::as_function()],
#'   it is called on the data in each group using the `x` coordinate as first and the `y`
#'   coordinate as second argument. If `fun` is a quosure, it is evaluated in the
#'   context of the data in the respective group.
#'
#'   `lotri_funtext()` and `utri_funtext()` are wrappers around
#'   [stat_funtext()] that additionally take care of the right specification of
#'   aesthetics. The positioning of the labels in [stat_funtext()] follows the
#'    same rules as in [stat_corrtext()].
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#' # function to compute linear model slopes
#' lmslope <- function(y, x)  round(coef(lm(y ~ x))[2], 2)
#'
#' # add slopes using a function
#' ggcorrm(drosera, rescale = "as_is") +
#'   lotri(geom_point(alpha = 0.4)) +
#'   utri_funtext(fun = lmslope) +
#'   dia_density(fill = "steelblue", lower = .4) +
#'   dia_names(y_pos = .1)
#'
#' # compute Pearson correlations as a lambda expression (top)
#' # and as a quosure(bottom)
#' ggcorrm(drosera, rescale = "as_is") +
#'   utri_funtext(fun = ~ round(cor(.x, .y), 3)) +
#'   lotri_funtext(fun = quo(round(cor(x, y), 3))) +
#'   dia_names(y_pos = .5)
#'
#' }
#' }
#'
#' @seealso [stat_funtext]
#' @name add_funtext
NULL

# lotri_funtext() ------------------------------------------------------------
#' @rdname add_funtext
#' @export
lotri_funtext <- function(fun, mapping = NULL,
                          nrow = NULL, ncol = NULL, squeeze = 0.7,
                          byrow = TRUE, show.legend = FALSE, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, y = y,
                                                 label = ..fun_out..))

  # return plot with labels
  lotri(
    stat_funtext(mapping = mapping, geom = "text", show.legend = show.legend,
                  fun = fun, ncol = ncol, nrow = nrow, squeeze = squeeze,
                 byrow = byrow, ...)
  )
}

# utri_funtext() -------------------------------------------------------------
#' @rdname add_funtext
#' @export
utri_funtext <- function(fun, mapping = NULL,
                         nrow = NULL, ncol = NULL, squeeze = 0.7,
                         byrow = TRUE,  show.legend = FALSE, ...) {
  # update and check mapping
  mapping <- update_aes_corrm(mapping,
                              standard_aes = aes(x = x, y = y, label = ..fun_out..))

  # return plot with labels
  utri(
    stat_funtext(mapping = mapping, geom = "text", show.legend = show.legend,
                 fun = fun, ncol = ncol, nrow = nrow, squeeze = squeeze,
                 byrow = byrow, ...)
  )
}

