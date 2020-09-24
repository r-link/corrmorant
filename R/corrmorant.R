#' @title Simple automated correlation plots
#'
#' @description `corrmorant()` is a wrapper around [ggcorrm()] that creates
#'   scatterplot matrices with correlation output using reasonable standard
#'   values for initial data inspection.
#'
#' @param style Character string defining the plot style. One of `"dark"`,
#'   `"light"`, `"blue_red"` or `"binned"`.
#' @param bins Integer. Number of bins for `style = "binned"` (ignored if using
#'   other styles). Defaults to 10.
#' @param ... Additional arguments to [ggcorrm()].
#' @inheritParams tidy_corrm
#'
#'
#' @details `corrmorant()` is a simplified wrapper around [ggcorrm()] that
#'   creates scatterplot matrices with reasonable standard settings. Setting the
#'   `style` argument to `"dark"`, `"light"` or `"blue_red"` produces regular
#'   scatterplots with different color schemes. `style = "binned"` replicates
#'   `style = "blue_red"` with a binned scatterplot (the number of bins defaults
#'   to 10, but can be changed with the `bins` argument), which can be very
#'   useful to reduce the plotting time for large datasets. Refer to the
#'   documentation of [ggcorrm()] and [tidy_corrm()] for more details.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # plot the drosera dataset with 3 different styles
#'  corrmorant(drosera, style = "dark")
#'  corrmorant(drosera, style = "light")
#'  corrmorant(drosera, style = "blue_red")
#'  }
#' }
#' @seealso [ggcorrm], [tidy_corrm]
#' @rdname corrmorant
#' @export
corrmorant <- function(data,
                       style = c("blue_red", "dark", "light", "binned"),
                       rescale     = c("as_is", "by_sd", "by_range"),
                       corr_method = c("pearson", "kendall", "spearman"),
                       labels      = NULL,
                       bins = 10,
                       ...){
  # match arguments
  rescale     <- rlang::arg_match(rescale)
  style       <- rlang::arg_match(style)
  corr_method <- rlang::arg_match(corr_method)

  # prepare plot
  p0 <- ggcorrm(data,
                rescale = rescale,
                bg_dia =  switch(style, dark = "grey20", NULL),
                corr_method = corr_method,
                labels = labels,
                ...)

  # prepare layers
  layers <- switch(
    style,
    blue_red = list(
      lotri(
        geom_point(mapping = aes(col = .corr),
                   alpha   = min(1 / log10(nrow(data)), 1))
      ),
      utri_corrtext(mapping = aes(col = .corr)),
      dia_density(lower  = .3,
                  fill   = "grey80",
                  col    = 1),
      dia_names(y_pos  = .15,
                colour = "black",
                size   = 3),
      scale_color_corr()
    ),
    dark = list(
      lotri(
        geom_point(alpha = min(1 / log10(nrow(data)), 1))
      ),
      utri_corrtext(),
      dia_density(lower = .3,
                  fill  = "grey90",
                  col   = 1),
      dia_names(y_pos  = .15,
                colour = "white",
                size   = 3)
    ),
    light = list(
      lotri(
        geom_point(alpha = min(1 / log10(nrow(data)), 1))
      ),
      utri_corrtext(),
      dia_density(lower = .3,
                  fill  = "grey80",
                  col   = 1),
      dia_names(y_pos  = .15,
                colour = "black",
                size   = 3)
    ),
    binned = list(
      lotri(stat_bin2d(aes(col = .corr, size = after_stat(density)),
                       geom = "point",
                       alpha = 0.666,
                       bins = bins,
                       fill = NA,
                       show.legend = c(size = FALSE, colour = TRUE))),
      utri_corrtext(mapping = aes(col = .corr)),
      dia_density(lower  = .3,
                  fill   = "grey80",
                  col    = 1),
      dia_names(y_pos  = .15,
                colour = "black",
                size   = 3),
      scale_color_corr()
    )
  )

  # return output
  p0 + layers
}
