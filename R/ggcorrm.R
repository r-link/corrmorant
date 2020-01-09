#' @title Create a ggcorrm correlation plot
#' @description \code{ggcorrm()} initializes a \code{ggcorrm} object (inheriting
#'     from class \code{ggplot}). It can be called either using the raw data for
#'     the correlation plot as input, which is then internally passed to
#'     \code{\link{tidy_corrm()}}, or with a prepared correlation dataset of class
#'     \code{tidy_corrm}.
#' @param data Dataset used to compute the correlation matrix. Can be either a
#'     \code{data.frame} or \code{matrix}, or an object of class \code{tidy_corrm}.
#'     If specifying a \code{data.frame} or \code{matrix}, it will internally be
#'     passed to \code{\link{tidy_corrm()}} with the settings for \code{labels},
#'     \code{rescale}, \code{corr_group}, \code{corr_method} and \code{mutates}
#'     specified in the \code{ggcorrm()} call. \code{\link{tidy_corrm()}}
#'     prepares the data for plotting by creating a \code{data.frame} with
#'     all possible combinations of all numeric variables, while retaining all
#'     discrete variables in additional columns. If a \code{tidy_corrm} object
#'     is supplied, all arguments passed to \code{\link{tidy_corrm()}} will be
#'     ignored and the \code{tidy_corrm} object wil be used directly to
#'     initialize the \code{ggcorrm} plot.
#' @param mapping Set of aesthetic mappings created by
#'    \code{\link[ggplot2:aes]{aes()}} that are passed on to subsequent layers.
#'    \code{x} and \code{y} are set automatically and must not be changed,  but
#'     all other aesthetics may be manipulated. Defaults to \code{NULL}
#'     (use standard \code{ggcorrm} mapping).
#' @param labels (Optional) character vector with labels for the names of all
#'     numeric columns that are used to replace the column names in the plot
#'     axis and text labels. Must be of the same length as the number of
#'     numeric columns displayed in the plot. Defaults to \code{NULL} (use
#'     original column names as labels).
#' @param rescale character string specifying the type of transformation
#'     performed on the numeric variables in the plot. The standard argument
#'     \code{"by.sd"} scales by the standard deviation of the data and centers
#'     around zero.  \code{"by.range"} rescales the range of the data to the
#'     interval from 0 to 1. Use \code{rescale = NULL} to use the unchanged raw
#'     values. Defaults to \code{"by_sd"}.
#' @param corr_method character string with the correlation method passed t
#'     \code{\link[stats]{cor}}. Used for the \code{.corr} variable appended to
#'     the \code{tidy_corr} dataset and passed on to
#'     \code{\link[geom_cortext]{geom_cortext()}} layers. Can be one of
#'     \code{"pearson"}, \code{"kendall"}  and \code{"spearman"}. Defaults to
#'     \code{"pearson"}.
#' @param corr_group \code{NULL} or the name of a numeric variable in \code{data}.
#'     If a grouping variable is specified, \code{.corr} will be calculated
#'     separately for each of these groups (which may be useful for conditional
#'     coloring). Defaults to \code{NULL}.
#' @param mutates (Optional) list of named quosures created with
#'     \code{\link[rlang:quos]{rlang::quos}}. Can be any expressions that specify
#'      changes to the `tidy_corrm` dataset \emph{after} reshaping, using regular
#'     \code{\link[dplyr:mutate]{dplyr::mutate}} syntax. Defaults to \code{NULL}
#'     (no \code{mutate} operations on the raw data).
#' @param bg_dia (Optional) background color specification for the diagonal panels.
#'     Either a character string with a hexadecimal color code, a character string
#'     specifying a color name in \code{\link[grDevices]{colors}}, or an integer
#'     specifying a position in \code{\link[grDevices]{palette}}. The default value
#'     of \code{NULL} uses the standard background color defined in the corresponding
#'     ggplot2 theme.
#' @param bg_lotri (Optional) background color specification for the panels in the
#'     lower triangle. Either a character string with a hexadecimal color code, a
#'     character string specifying a color name in \code{\link[grDevices]{colors}},
#'     or an integer specifying a position in \code{\link[grDevices]{palette}}. The
#'     default value of \code{NULL} uses the standard background color defined in the
#'     corresponding ggplot2 theme.
#' @param bg_utri (Optional) background color specification for the panels in the
#'     lower triangle. Either a character string with a hexadecimal color code, a
#'     character string specifying a color name in \code{\link[grDevices]{colors}},
#'     or an integer specifying a position in \code{\link[grDevices]{palette}}. The
#'     default value of \code{NULL} uses the standard background color defined in the
#'     corresponding ggplot2 theme.
#' @details \code{ggcorrm} creates the initial correlation plot object containing
#'     information about panel placement, correlations, themes etc. Its output is a
#'     modified empty \code{ggplot} object with appropriate facet and theme
#'     specifications. If a \code{tidy_corrm} object is supplied as \code{data},
#'     it will be directly plotted without invoking \code{link{tidy_corrm}}, else,
#'     \code{ggcorrm} passes the raw data and additional arguments to
#'     \code{link{tidy_corrm}} before plotting (see documentation of this function
#'     for details).
#'
#'     New layers can be added using classical
#'     \code{\link[ggplot:ggplot_add]{ggplot::ggplot_add}}
#'     syntax, though in most cases it will be more useful to add layers using the
#'     \code{\link[corrmorant_selectors]{corrmorant selector functions}} which allow
#'     to map geoms to a subset of panels on the plot diagonal, lower or upper
#'     triangle using \code{dia()}, \code{lotri()} or \code{utri()}, respectively
#'     (see examples).
#'
#'     \code{bg_dia}, \code{bg_lotri} and \code{bg_utri} allow to specify different
#'     background color settings for the plot diagonal, the lower and the upper
#'     triangle of the correlation plot matrix, respectively. All other graphics
#'     settings can be modified using regular ggplot2 \code{\link[ggplot2]{theme}}
#'     syntax, building upon the corrmorant standard theme
#'     (\code{\link{theme_corrm}}).
#'
#' @return An object of class \code{ggcorrm} containing the reshaped dataset for the
#'     correlation plot and an empty \code{ggplot} object with appropriate facet and
#'     theme specifications.
#' @examples
#' \dontrun{
#' if(interactive()){
#' # correlation matrix for the iris dataset
#' ggcorrm(iris, bg_dia = "grey20") +
#'   lotri(geom_point(alpha = 0.4)) +
#'   utri_corrtext() +
#'   dia_histogram(lower = .3, fill = "grey90", col = 1) +
#'   dia_names(y_pos = .1, col = "white", size = 3)
#'
#' # iris data with conditional coloring by pearson correlation
#' ggcorrm(iris, aes(col = .corr, fill = .corr)) +
#'   lotri(geom_point(alpha = 0.6)) +
#'   lotri(geom_smooth(method = "lm", size = 0.3, alpha = 0.6)) +
#'   utri_corrtext() +
#'   dia_density(fill = "grey80", col = 1, lower = .4) +
#'   dia_names(y_pos = .1) +
#'   scale_color_corr(option = "A", aesthetics = c("fill", "color"))
#'
#' # correlation separated by groups
#' ggcorrm(iris, aes(col = Species, fill = Species),
#'         rescale = "by_sd", bg_dia = "grey95") +
#'   lotri(geom_point(alpha = 0.4)) +
#'   lotri(geom_smooth(col = 1, method = "lm"))  +
#'   utri_corrtext(nrow = 2) +
#'   dia_density(col = 1, alpha = 0.5, lower = 0.4) +
#'   dia_names(y_pos = 0.15)
#'
#' # using the 'mutates' argument to color diagonal panels by leaf type
#' ggcorrm(iris, rescale = "by_sd",
#'         mutates = quos(leaftype = ifelse(substr(var_x, 1, 1) == "S",
#'                                          "Sepal", "Petal"))) +
#'   lotri(geom_point(alpha = 0.4))+
#'   utri_corrtext() +
#'   dia_density(lower = .3,
#'               mapping = aes(fill = leaftype)) +
#'   dia_names(y_pos = .1,
#'             mapping = aes(col = leaftype))
#'  }
#' }
#' @seealso
#'  \code{\link{tidy_corrm}},
#'  \code{\link{corrmorant}},
#'  \code{\link[ggplot2:ggplot]{ggplot2::ggplot}},
#'  \code{\link[ggplot2:theme]{ggplot2::theme}},
#'  \code{\link[dplyr:mutate]{dplyr::mutate}},
#'  \code{\link[rlang:quos]{rlang::quos}}
#' @rdname ggcorrm
#' @export
#' @importFrom dplyr tibble
ggcorrm <- function(data,
                    mapping = NULL,
                    labels = NULL,
                    rescale = c("by_sd", "by_max", NULL),
                    corr_method = "pearson",
                    corr_group = NULL,
                    mutates = NULL,
                    bg_dia = NULL,
                    bg_lotri = NULL,
                    bg_utri = NULL){
  # control class of data
  if (!inherits(data, "data.frame") | is.matrix(data)) {
    stop("data must be a data.frame or matrix.")
  }
  # check if mapping is appropriate
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in ggcorrm() may not be manipulated.")
  }

  # if rescale argument is not changed, pick first
  rescale <- match.arg(rescale)
  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!is_quosures(mutates))
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }

  # if data is already in correct form, do not change...
  if (is(data, "tidy_corrm")){
    corrdat <- data
  } else { # ...else reshape to appropriate format
  # catch grouping variable
  corr_group <- enquo(corr_group)
  # prepare data
  corrdat <- tidy_corrm(data,
                        labels      = labels,
                        rescale     = rescale,
                        corr_group  = corr_group,
                        corr_method = corr_method,
                        mutates     = mutates)
  }

  # update mapping
  new_mapping <- modify_list(aes(x = x, y = y), mapping)

  # prepare plot
  plot_out <- ggplot(data = corrdat, mapping = new_mapping,
                     corr_method = corr_method, corr_use = corr_use) +
    facet_grid(var_x ~ var_y, scales = "free") +
    geom_blank() + # add geom_blank to set dimensions
    theme_corrm()

  # add background layer if desired
  if(any(!is.null(list(bg_dia, bg_lotri, bg_utri)))){
    bgdat <- dplyr::tibble(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    bgs <- vector(mode = "list", length = 3)
    if(!is.null(bg_dia)){
      bgs[[1]] <- dia(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                data = bgdat, fill = bg_dia, inherit.aes = FALSE))
    }
    if(!is.null(bg_lotri)){
      bgs[[2]] <- lotri(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                  data = bgdat, fill = bg_lotri, inherit.aes = FALSE))
    }
    if(!is.null(bg_utri)){
      bgs[[3]] <- utri(geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                 data = bgdat, fill = bg_utri, inherit.aes = FALSE))
    }
    plot_out <- plot_out + bgs
  }

  # return plot with updated class
  return(structure(plot_out,
                   class = c(class(plot_out), "ggcorrm")))
}
