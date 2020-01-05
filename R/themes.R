#' @title Themes for ggcorrm plots
#' @description ggplot themes controlling all non-data display meant
#'     for the use with \code{ggcorrm}. Single elements of a theme can
#'     be modified with \code{\link[ggplot2:theme]{ggplot2::theme()}}.
#' @param base_size Base font size (defaults to 9).
#' @param base_family Base font family (defaults to "").
#' @param base_line_size Base size for line elements (defaults to base_size/22).
#' @param base_rect_size Base sie for rect elements (defaults to base_size/22).
#' @details Available themes:
#'     \describe{
#'         \item{theme_corrm()}{The standard corrmorant theme with white background and
#'               strong black facet outlines. \emph{So far the only available
#'               corrmorant theme.}
#'               }
#'            }
#' @seealso
#'   \code{\link[ggplot2:ggtheme]{ggplot2::ggtheme}},
#'   \code{\link[ggplot2:theme]{ggplot2::theme}},
#'   \code{\link{ggcorrm}},
#'   \code{\link{tidy_corrm}},
#'   \code{\link{corrmorant}}
#' @name corrmoran_themes
NULL


#' @rdname corrmoran_themes
#' @export
#' @importFrom grid unit
theme_corrm <- function(base_size = 9, base_family = "",
                        base_line_size = base_size/22,
                        base_rect_size = base_size/22) {
  # built upon theme_grey (standard ggplot theme)
  theme_grey(base_size,
             base_family,
             base_line_size,
             base_rect_size) %+replace%
    theme(aspect.ratio = 1,
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = grid::unit(0, "mm"),
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          strip.background = element_blank()
          )
}
