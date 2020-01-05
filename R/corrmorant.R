#' @title Simple automated correlation plots
#' @description \code{corrmorant()} is a wrapper around \code{\link[ggcorrm]{ggcorrm()}}
#'    that creates scatterplot matrices with correlation output using
#'    reasonable standard values for initial data inspection.
#' @param style Character string defining the plot style. One of "dark", "light"
#'      or "blue_red".
#' @param ... Additional arguments to \code{\link{ggcorrm}()}.
#' @inheritParams tidy_corrm
#' @return An object of class \code{ggcorrm}.
#' @details \code{corrmorant()} is a simplified wrapper around \code{\link[ggcorrm]{ggcorrm()}}
#'    that creates scatterplot matrices with reasonable standard settings.
#'    Refer to the documentation of\code{\link[ggcorrm]{ggcorrm()}} and
#'    \code{\link[tidy_corrm]{tidy_corrm()}} for details.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # plot the iris dataset with 3 different styles
#'  corrmorant(iris, style = "dark")
#'  corrmorant(iris, style = "light")
#'  corrmorant(iris, style = "blue_red")
#'  }
#' }
#' @seealso
#'   \code{\link{ggcorrm}},
#'   \code{\link{tidy_corrm}}
#' @rdname corrmorant
#' @export
corrmorant <- function(data, style = c("dark", "light", "blue_red"),
                       rescale     = c("by_sd", "by_range", NULL),
                       labels       = NULL,
                       ...){
  # match arguments
  rescale <- match.arg(rescale)
  style   <- match.arg(style)

  # prepare plot
  p0 <- ggcorrm(data,
                rescale = rescale,
                bg_dia =  switch(style, dark = "grey20", NULL),
                labels = labels,
                ...)

  # prepare layers
  layers <- list(
  lotri(geom_point(alpha = min(1 / log10(nrow(data)), 1),
                   mapping = switch(style, blue_red = aes(col = .corr), NULL))),
  utri(geom_corrtext(mapping = switch(style, blue_red = aes(col = .corr), NULL))),
  dia_density(lower = .4, fill = switch(style, dark = "grey90", "grey80"), col = 1),
  dia_names(y_pos = .1, colour = switch(style, dark = "white", "black"), size = 3),
  switch(style, blue_red = scale_color_corr(option = "A",
                                            aesthetics = c("fill", "color")), NULL)
  )

  # return output
  p0 + layers
}
