# this objects exists only to make use of ggproto inheritance rules
# to avoid dependencies from ggplot internals
#' @noRd
#' @export
StatFuntextProto <- ggproto("StatFuntextProto", Stat,
    required_aes = c("x", "y"),
   # compute_group
   compute_group = function (data, scales,
                             fun,
                             nrow = NULL, ncol = NULL,
                             squeeze = 0.7, ...){
     if(rlang::is_quosure(fun)){
       data <- dplyr::summarize(data, fun_out = !!fun)
     } else {
       data <- dplyr::summarize(data, fun_out = as_function(fun)(x, y))
     }
     data
   }
)


# Statfuntext - ggproto object for stat_funtext -------------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFuntext <- ggproto("StatFuntext", StatFuntextProto,
   required_aes = c("x", "y"),
   # compute panel - standard function just slightly updated to pass ranges
   compute_panel = function (self, data, scales, fun,
                             nrow = NULL, ncol = NULL,
                             squeeze = 0.7, ...) {
     stats <- StatFuntextProto$compute_panel(data = data,
                                             scales = scales,
                                             fun = fun,
                                             ...)
     # rescale output after computation
     get_corrtext_pos(stats = stats,
                      nrow = nrow, ncol = ncol,
                      squeeze = squeeze,
                      xrange = scales$x$get_limits(),
                      yrange = scales$y$get_limits())
   },
   # will not be evaluated, but argument names are needed:
   compute_group =  function (data, scales,fun,
                              nrow, ncol, squeeze = 0.7, ...){
     StatFuntextProto$compute_group(data, scales,fun,
                                    nrow, ncol, squeeze = 0.7,
                                    ...)
   },
   setup_data = function(data, params){
     # check if fun specification is valid
     if (!is.function(params$fun) &&
         !rlang::is_formula(params$fun) &&
         !rlang::is_quosure(params$fun)) {
       stop("fun argument in stat_funtext() must be a function, quosure or lambda expression.\n")
     }

     # check number of groups
     grouptab <- dplyr::group_by(data, PANEL) %>%
       dplyr::summarize(n = length(unique(group)))
     if (any(grouptab$n > 9)) {
       warning("stat_funtext() uses a very large number of groups per panel.\n",
               "Is this really what you want to do?")
     }
     data
   }
)

# stat_funtext() - stat function based on funtext -----------------------------
#' @title Compute text labels based on user-specified functions
#'
#' @description `stat_funtext()` is used to compute
#'   the facets of [ggcorrm] plots.
#'
#' @inheritParams ggcorrm
#' @inheritParams add_corrtext
#' @inheritParams ggplot2::layer
#' @param fun One of the following: * a) a function of `x` and `y`. * b) an
#'   rlang lambda style [one-sided formula][rlang:as_function] describing a
#'   function of two parameters labeled`.x` and `.y` * c) an rlang quosure
#'   created with `quo()` that is evaluated in the context of the data analogous
#'   to the `mutates` argument in [tidy_corrm()].
#'
#'   The functions in a) and b) are evaluated using the `x` and `y`coordinates
#'   of the plot as first and second argument, respectively. The quosure in c)
#'   is evaluated in the context of the raw data and may contain x and y.
#'
#'   One outcome of the computation per group is stored in a new column called
#'   `fun_out` that can be accessed in the aesthetics using`..fun_out..`.
#' @param ... additional arguments passed to [ggplot2::layer()].
#'
#' @return An object of class `Layer`.
#'
#' @details `stat_funtext()` evaluates a function, rlang lambda style [one-sided
#'   formula][rlang:as_function] or a quosure in the context of the data used
#'   for plotting and computes positions in the plot using the same
#'   `nrow`/`ncol` based positioning as [stat_corrtext()].
#'
#' @rdname stat_funtext
#'@seealso
#'  [stat_corrtext()] for correlation text labels.
#'  [lotri_funtext()] and [utri_funtext()]

#' @export
stat_funtext <- function(mapping = NULL, data = NULL, geom = "text",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, fun,
                         nrow = NULL, ncol = NULL,
                         squeeze = 0.7,
                         ...) {
  layer(
    stat = StatFuntext, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun = fun, squeeze = squeeze,
                  nrow = nrow, ncol = ncol, ...)
  )
}
