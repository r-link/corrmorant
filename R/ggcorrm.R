#' @title Create a ggcorrm correlation plot
#'
#' @description `ggcorrm()` initializes a `ggcorrm` object (inheriting from
#'   class `ggplot`). It can be called either using the raw data for the
#'   correlation plot as input, which is then internally passed to
#'   [tidy_corrm()], or with a prepared correlation dataset of class
#'   `tidy_corrm`.
#'
#' @param data Dataset used to compute the correlation matrix. Can be either a
#'   `data.frame` or `matrix`, or an object of class `tidy_corrm`. If specifying
#'   a `data.frame` or `matrix`, it will internally be passed to [tidy_corrm()]
#'   with the settings for `labels`, `rescale`, `corr_group`, `corr_method` and
#'   `mutates` specified in the `ggcorrm()` call. [tidy_corrm()] prepares the
#'   data for plotting by creating a `data.frame` with all possible combinations
#'   of all numeric variables, while retaining all discrete variables in
#'   additional columns. If a `tidy_corrm` object is supplied, all arguments
#'   passed to [tidy_corrm()] will be ignored and the `tidy_corrm` object wil be
#'   used directly to initialize the `ggcorrm` plot.
#' @param mapping Set of aesthetic mappings created by [aes][ggplot2::aes] that
#'   are passed on to subsequent layers. `x` and `y` are set automatically and
#'   must not be changed,  but all other aesthetics may be manipulated. Defaults
#'   to `NULL` (use standard `ggcorrm` mapping).
#' @param labels (Optional) character vector with labels for the names of all
#'   numeric columns that are used to replace the column names in the plot axis
#'   and text labels. Must be of the same length as the number of numeric
#'   columns displayed in the plot. Defaults to `NULL` (use original column
#'   names as labels).
#' @param rescale character string specifying the type of transformation
#'   performed on the numeric variables in the plot. The standard argument
#'   `"by.sd"` scales by the standard deviation of the data and centers around
#'   zero.  `"by.range"` rescales the range of the data to the interval from 0
#'   to 1. Use `rescale = "as_is"` to use the unchanged raw values. Defaults to
#'   `"by_sd"`.
#' @param corr_method character string with the correlation method passed to
#'   [stats::cor()]. Used for the `.corr` variable appended to the `tidy_corr`
#'   dataset and passed on to [lotri_corrtext()]/ [utri_corrtext()]  layers. Can
#'   be one of `"pearson"`, `"kendall"`  and `"spearman"`. Defaults to
#'   `"pearson"`.
#' @param corr_group `NULL` or the name of a numeric variable in `data`. If a
#'   grouping variable is specified, `.corr` will be calculated separately for
#'   each of these groups (which may be useful for conditional coloring).
#'   Defaults to `NULL`.
#' @param mutates (Optional) list of named quosures created with
#'   [rlang::quos()]. Can be any expressions that specify changes to the
#'   `tidy_corrm` dataset _after_ reshaping, using regular [dplyr::mutate()]
#'   syntax. Defaults to `NULL` (no `mutate` operations on the raw data).
#' @param bg_dia (Optional) background color specification for the diagonal
#'   panels. Either a character string with a hexadecimal color code, a
#'   character string specifying a color name in [colors][grDevices::colors], or
#'   an integer specifying a position in [palette][grDevices::palette]. The
#'   default value of `NULL` uses the standard background color defined in the
#'   current ggplot2 theme.
#' @param bg_lotri (Optional) background color specification for the panels in
#'   the lower triangle. Either a character string with a hexadecimal color
#'   code, a character string specifying a color name in
#'   [colors][grDevices::colors], or an integer specifying a position in
#'   [palette][grDevices::palette]. The default value of `NULL` uses the
#'   standard background color defined in the current ggplot2 theme.
#' @param bg_utri (Optional) background color specification for the panels in
#'   the lower triangle. Either a character string with a hexadecimal color
#'   code, a character string specifying a color name in
#'   [colors][grDevices::colors], or an integer specifying a position in
#'   [palette][grDevices::palette]. The default value of `NULL` uses the
#'   standard background color defined in the current ggplot2 theme.
#' @param facet_arg (Optional) list with additional arguments for the
#'   [facet_grid()][ggplot2::facet_grid()] call that defines the structure of
#'   the panels of the correlation matrix.
#'
#' @details `ggcorrm` creates the initial correlation plot object containing
#'   information about panel placement, correlations, themes etc. Its output is
#'   a modified empty `ggplot` object with appropriate facet and theme
#'   specifications. If a `tidy_corrm` object is supplied as `data`, it will be
#'   directly plotted without invoking [tidy_corrm()], else, `ggcorrm()` passes
#'   the raw data and additional arguments to [tidy_corrm()] before plotting
#'   (see documentation of this function for details).
#'
#'   New layers can be added using standard [ggplot::ggplot_add()] syntax,
#'   though in most cases it will be more useful to add layers using the
#'   [corrmorant selector functions][corrmorant_selectors] which allow to map
#'   geoms to a subset of panels on the plot diagonal, lower or upper triangle
#'   using `dia()`, `lotri()` or `utri()`, respectively (see examples).
#'
#'   `bg_dia`, `bg_lotri` and `bg_utri` allow to specify different background
#'   color settings for the plot diagonal, the lower and the upper triangle of
#'   the correlation plot matrix, respectively. All other graphics settings can
#'   be modified using regular ggplot2 [theme][ggplot2::theme] syntax, building
#'   upon the corrmorant standard theme ([theme_corrm]).
#'
#'   `facet_arg` allows to change the settings in the [ggplot2::facet_grid()]
#'   call underlying the facet structure of a corrmorant plot. This is likely
#'   most helpful if you wish to parse facet labels as expressions with
#'   `facet_arg = list(labeller = "label_parsed)`.
#'
#' @return An object of class `ggcorrm` containing the reshaped dataset for the
#'   correlation plot and an empty `ggplot` object with appropriate facet and
#'   theme specifications.
#'
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
#'  * [tidy_corrm()] for the preparation of tidy corrmorant datasets
#'  * [corrmorant()] for a fast and easy to use version of `ggcorrm()`
#' @rdname ggcorrm
#' @export
#' @importFrom dplyr tibble
ggcorrm <- function(data,
                    mapping = NULL,
                    labels = NULL,
                    rescale = c("by_sd", "by_max", "as_is"),
                    corr_method = c("pearson", "kendall", "spearman"),
                    corr_group = NULL,
                    mutates = NULL,
                    bg_dia = NULL,
                    bg_lotri = NULL,
                    bg_utri = NULL,
                    facet_arg = NULL
                    ){
  # control class of data
  if (!(inherits(data, "data.frame") | is.matrix(data))) {
    stop("data must be a data.frame or matrix.")
  }
  # check if mapping is appropriate
  if (any(c("x", "y") %in% names(mapping))) {
    stop("x and y coordinates in ggcorrm() may not be manipulated.")
  }

  # match arguments
  rescale     <- rlang::arg_match(rescale)
  corr_method <- rlang::arg_match(corr_method)

  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!is_quosures(mutates))
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }

  # if data is already in correct form, keep as is and adjust global
  # corr_method setting accordingly
  if (is(data, "tidy_corrm")){
    if (any(c(corr_method != "pearson",
              rescale != "by_sd",
              !is.null(labels),
              !is.null(corr_group),
              !is.null(mutates)))) {
      warning("All tidy_corrm() arguments are ignored if data is a tidy_corrm object.",
              call. = FALSE)
    }
    corrdat <- data
    # get correlation settings
    corr_method <- attr(data, "corr_method")
    corr_group  <- attr(data, "corr_group")
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

  # prepare layers
  layers <- make_corrm_layers(backgrounds = list(bg_dia, bg_lotri, bg_utri))

  # get facet arguments
  facet_arg <- modify_list(list(rows = var_x ~ var_y, scales = "free"), facet_arg)

  # prepare output
  plot_out <- structure(list(
    data = corrdat,
    layers = layers,
    scales = ggplot()$scales, # cheap hack, must be improved
    mapping = new_mapping,
    theme = theme_corrm(),
    coordinates = coord_cartesian(default = TRUE),
    facet = do.call(what = facet_grid, args = facet_arg),
    plot_param = list(corr_method = corr_method, corr_group = corr_group),
    plot_env = parent.frame()
  ), class = c( "ggcorrm", "gg", "ggplot"))

  # get axis labels and scale names
  plot_out$labels <- make_labels(new_mapping)

  # update graphics device
  set_last_plot(plot_out)

  # return output
  plot_out
}

#' @keywords internal
make_corrm_layers <- function(backgrounds){
  # add initial empty layer (for correct dimensions)
  layers <- list(geom_blank())
  # get specified background colors
  include <- !sapply(backgrounds, is.null)
  # create specified bacground layers
  if(any(include)){
    selectors <- list(dia, lotri, utri)
    bg <- mapply(FUN      = make_corrm_background,
                 backgrounds[include],
                 selectors[include])
    layers <- c(layers, bg)
  }
  # return
  layers
}

#' @keywords internal
make_corrm_background <- function(fill, selector){
    # create background layer for ggcorrm plots
    selector(
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                data = data.frame(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = fill, inherit.aes = FALSE)
    )
}
