# TODO: implement reordering by clustering algorithm as in corrplot

#' @title Reshape data for correlation matrices to a tidy format
#' @description \code{tidy_corrm()} takes a \code{data.frame} or \code{matrix}
#'    and reshapes it to a long-table format that enables plotting with
#'    \code{\link[ggcorrm]{ggcorrm()}}.
#' @param data \code{data.frame} or \code{matrix} with the raw data used
#'    for the correlation plot. If a \code{data.frame}, all numeric variables
#'    are used as rows/columns of the correlation plot, while all other
#'    variables are appended to the reshaped dataset as additional columns.
#' @inheritParams ggcorrm
#' @return An object of class \code{tidy_corrm} (a tibble with structured
#'    correlation data) containing the following columns:
#'    \describe{
#'      \item{var_x}{Name of the variable on the x-axis in
#'        the order of appearance in the raw data (ordered factor).}
#'      \item{var_y}{Name of the variable on the y-axis in
#'        the order of appearance in the raw data (ordered factor)}
#'      \item{x}{Data of the variable on the x axis (numeric).}
#'      \item{y}{Data of the variable on the y axis (numeric).}
#'      \item{type}{Type of panel (character, \code{"upper"}, \code{"lower"} or
#'        \code{"diag"}).}
#'      \item{.corr}{Correlation between x and y for the respective panel/group,
#'        calculated with \code{\link[stats]{cor}} using
#'        the method specified by \code{corr_method} and optionally
#'        within the groups specified with \code{corr_group} (numeric).}
#'      \item{corr_group}{grouping variable for \code{.corr} (1 for all
#'      observations if no groups are specified).}
#'      \item{Additional columns}{All other columns specified in the dataset
#'        and/or created via \code{mutates}.}
#'      }
#'
#' @details \code{tidy_corrm()} is used to reshape a \code{data.frame} or
#'    \code{matrix} with raw data for a correlation plot to a long-table
#'    format that can be plotted with \code{\link[ggcorrm]{ggcorrm()}}. The function
#'    creates a tibble with all combinations of all numeric variables in the
#'    dataset that are labelled with their column names (or, alternatively,
#'    a vector with new labels) in the order of their appearance in the raw
#'    data. All other variables are appended to the reshaped data.frame and
#'    can be accessed in the plots.
#'
#'    By default, the data are scaled and centered using their standard
#'    deviation (\code{rescale = "by.sd"}), but it is also possible to
#'    rescale them into the range from 0 to 1 (\code{rescale = "by.range"}) or
#'    to maintain the original scale of the data (\code{rescale = NULL}).
#'
#'    An additional variable called \code{.corr} with the bivariate correlation
#'    of the two variables (by default, Pearson correlation,
#'    see \code{\link[stats]{cor}}) is appended to the dataset. This variable
#'    can e.g. be used to specify the colour or fill of geoms conditional of
#'    the strength of the correlation (see examples in \code{\link{ggcorrm}}).
#'    If the correlations displayed with \code{\link{lotri_corrtext}} or
#'    \code{\link{utri_corrtext}} are separated
#'    by groups, it may make sense to also calculate \code{.corr} separately for
#'    these groups. In this case, it is possible to specify a grouping variable
#'    for the calculation of \code{.corr} using \code{corr_group}.
#'
#'    In many cases, the variables in the correlation matrix belong to
#'    different groups of variables. As the input for \code{tidy_corrm} is
#'    based on a wide table format, it is usually not possible to include this
#'    information as an additional column in the raw data. There are two ways to
#'    include variable-specific information \emph{after the fact}: a)
#'    \code{tidy_corrm()} can be called directly, and its output can be modified
#'    manually before passing it to \code{ggcorrm()} or b) the \code{mutates}
#'    argument can be used to pass a list of named quosures created with
#'    \code{\link[rlang:quos]{rlang::quos}} that contain a set of mutating
#'    operations based on regular \code{\link[dplyr:mutate]{dplyr::mutate}}
#'    syntax that are evaluated inside the reshaped dataset (see examples).
#'    For the standard column names of \code{tidy_corr} objects see the Value
#'    section.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'    # general shape of the output
#'    corrdat <- tidy_corrm(iris)
#'    head(corrdat)
#'
#'    # relabeling variables
#'    corrdat1 <- tidy_corrm(iris, labels = c("Some", "very", "nice", "labels"))
#'    head(corrdat1)
#'
#'    # use of mutates argument
#'    corrdat2 <- tidy_corrm(iris,
#'                mutates = quos(leaf_type  = substr(var_x, 1, 5),
#'                                dimension = substr(var_x, 7, 13)
#'                                )
#'                 )
#'    head(corrdat2)
#'  }
#' }
#' @seealso
#'  \code{\link{ggcorrm}},
#'  \code{\link{corrmorant}},
#'  \code{\link[ggplot2:ggplot]{ggplot2::ggplot}},
#'  \code{\link[ggplot2:theme]{ggplot2::theme}},
#'  \code{\link[dplyr:mutate]{dplyr::mutate}},
#'  \code{\link[rlang:quos]{rlang::quos}}
#' @rdname tidy_corrm
#' @export
#' @importFrom dplyr mutate_if mutate group_nest select arrange group_by ungroup case_when
#' @importFrom stats sd
#' @importFrom tidyr gather expand_grid unnest
#' @importFrom purrr map
tidy_corrm <- function(data,
                       labels = NULL,
                       rescale = c("by_sd", "by_range", NULL),
                       corr_method = "pearson",
                       corr_group = NULL,
                       mutates = NULL) {
  # control class of data
  if (!inherits(data, "data.frame") | is.matrix(data)) {
    stop("data must be a data.frame or matrix.")
  }

  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!is_quosures(mutates))
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }

  # if data is a matrix, convert to data.frame
  if (is.matrix(data)) data <- as.data.frame(data)

  # replace infinite values and NaN values by NA values to avoid problems
  # during transformation and when calculating correlations
  data[is.infinite(data)] <- NA
  data[is.nan(data)] <- NA

  # get column numbers of numeric variables
  numerics <- which(sapply(data, is.numeric))

  # if labels were specified, check if they are of the right form
  if(!is.null(labels)){
    if (length(labels) != length(numerics))
      stop("Number of labels must be equal to the number of numeric columns.")

    # print changes of column names
    cat("The following column names were replaced:\n",
        paste(names(data)[numerics], labels, sep = "\t->\t", collapse = "\n"),
        "\n",  sep = "")

    # reset names of numeric columns
    names(data)[numerics] <- labels
  }

  # print warning if there are more than 10 numeric variables
  if (length(numerics) > 9)
    warning("Correlation matrix contains ", length(numerics), " numeric variables:\n",
            "Plotting may take very long.\n",call. = FALSE)

  # print warning if there are highly skewed variables
  skews <- apply(data[, numerics], 2, skew)
  if (any(abs(skews) > 1))
    warning("Some variables are highly skewed (abs(skew) > 1).\n",
            "Consider transformation for better display.", call. = FALSE)

  # performe scale transformation if desired
  rescale <- match.arg(rescale)
  if (rescale == "by_sd") {
    data <- dplyr::mutate_if(data, is.numeric,
                             function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE))
  }
  if (rescale == "by_range") {
    data <- dplyr::mutate_if(data, is.numeric,
                             function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
  }

  # prepare grouping variable
  if(!is_quosure(corr_group)) corr_group <- enquo(corr_group)
  data <- dplyr::mutate(data, corr_group = !!corr_group)
  # if !!corr_group evaluated to NULL set uniform group
  if (!has_name(data, "corr_group")) data$corr_group <- 1

  # reshape dataset to long format
  longtab <- tidyr::gather(data, key = "var", value = "x", numerics) %>%
    # control order of levels
    dplyr::mutate(var = factor(var, levels = names(data)[numerics], ordered = TRUE)) %>%
    # group by variable and nest
    dplyr::group_nest(var) %>%
    # add reduced nested dataset for y
    dplyr::mutate(dat_y = purrr::map(data, ~ dplyr::select(.x, y = x))) %>%
    # assure correct oder (important for correct subsetting - CAN BE DANGEROUS)
    dplyr::arrange(var)

  # prepare plotting output (all x and y and identifiers for everything else)
  # get all combinations of x and y values
  out <- tidyr::expand_grid(var_x = longtab$var, var_y = longtab$var) %>%
    # create columns for the corresponding data
    dplyr::mutate(dat_x = longtab$data[as.numeric(var_y)],
                  dat_y = longtab$dat_y[as.numeric(var_x)]) %>%
    # unnest data columns
    tidyr::unnest(cols = c(dat_x, dat_y)) %>%
    # get column with correlations (for use in plots)
    dplyr::group_by(var_x, var_y, corr_group) %>%
    dplyr::mutate(.corr = cor(x, y, method = corr_method, use = "pairwise.complete.obs")) %>%
    dplyr::ungroup() %>%
    # get indicator for position
    dplyr::mutate(type = dplyr::case_when(var_x <  var_y ~ "upper",
                                          var_x >  var_y ~ "lower",
                                          var_x == var_y ~ "diag")) %>%
    dplyr::select(var_x, var_y, x, y, type, .corr, corr_group,
                  tidyr::everything())

  # if specified, perform desired mutations (must be list of quosures)
  if(!is.null(mutates)) out <- dplyr::mutate(out, !!!mutates)

  # return output with appended class (used for testing in corrmorant selectors)
  return(structure(out, class = c(class(out), "tidy_corrm")))
}
