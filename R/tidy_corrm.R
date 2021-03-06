# tidy_corrm generic ----------------------------------------------------------
# TODO: implement reordering by clustering algorithm as in corrplot

#' @title Reshape data for correlation matrices to a tidy format
#'
#' @description `tidy_corrm()` is a generic function with the purpose to take a
#'   dataset and reshape it to a long-table format that can be plotted with
#'   [ggcorrm()].
#'
#' @param data For `tidy_corrm.default`, a `data.frame` or `matrix` with the
#'   raw data used for the correlation plot. If a `data.frame`, all numeric
#'   variables are used as rows/columns of the correlation plot, while all other
#'   variables are appended to the reshaped dataset as additional columns.
#' @param ... Further arguments (currently ignored in `tidy_corrm.default`)
#' @inheritParams ggcorrm
#' @return An object of class `tidy_corrm` (a tibble with structured correlation
#'   data) containing the following columns:
#'
#'   \describe{ \item{var_x}{Name of the variable on the x-axis in the order of
#'   appearance in the raw data (ordered factor).}
#'
#'   \item{var_y}{Name of the variable on the y-axis in the order of appearance
#'   in the raw data (ordered factor)}
#'
#'   \item{x}{Data of the variable on the x axis (numeric).}
#'
#'   \item{y}{Data of the variable on the y axis (numeric).}
#'
#'   \item{pos}{Type of panel (character, `"utri"`, `"lotri"` or `"dia"`).}
#'
#'   \item{.corr}{Correlation between x and y for the respective panel/group,
#'   calculated with [cor()][stats::cor] using the method specified by
#'   `corr_method` and optionally within the groups specified with `corr_group`
#'   (numeric).}
#'
#'   \item{corr_group}{grouping variable for `.corr` (1 for all observations if
#'   no groups are specified).}
#'
#'   \item{Additional columns}{All other columns specified in the dataset and/or
#'   created via `mutates`.} }
#'
#'
#' @details `tidy_corrm()` is a generic S3 method that reshapes raw data for a
#'   correlation plot to a long-table format that can be plotted with
#'   [ggcorrm()]. The default method takes a `data.frame` or `matrix` and
#'   creates a tibble with all combinations of all numeric variables in the
#'   dataset that are labelled with their column names (or, alternatively, a
#'   vector with new labels) in the order of their appearance in the raw data.
#'   All other variables are appended to the reshaped data.frame and can be
#'   accessed in the plots.
#'
#'   By default, the data enter the plot unchanged  (`rescale = "as_is"`), but
#'   it is also possible to scale and center using their standard deviation
#'   (`rescale = "by_sd"`) or to rescale them into the range from 0 to 1
#'   (`rescale = "by_range"`).
#'
#'   An additional variable called `.corr` with the bivariate correlation of the
#'   two variables (by default, Pearson correlation, see [cor()][stats::cor()])
#'   is appended to the dataset. This variable can e.g. be used to specify the
#'   colour or fill of geoms conditional of the strength of the correlation (see
#'   examples in [ggcorrm()]). If the correlations displayed with
#'   [lotri_corrtext()] or [utri_corrtext()] are separated by groups, it may
#'   make sense to also calculate `.corr` separately for these groups. In this
#'   case, it is possible to specify a grouping variable for the calculation of
#'   `.corr` using `corr_group`.
#'
#'   In many cases, the columns of the `data.frame` used to construct the
#'   correlation matrix belong to different groups of variables. As the input
#'   for `tidy_corrm()` is based on a wide table format, it is often not easily
#'   possible to include this information as an additional column in the raw
#'   data. There are two ways to include variable-specific information _after
#'   the fact_: a) `tidy_corrm()` can be called directly, and its output can be
#'   modified manually before passing it to `ggcorrm()` or b) the `mutates`
#'   argument can be used to pass a list of named quosures created with
#'   [rlang::quos()][rlang::nse-defuse] that contain a set of mutating
#'   operations based on regular [dplyr::mutate()] syntax that are evaluated
#'   inside the reshaped dataset (see examples). For the standard column names
#'   of `tidy_corr` objects see the Value section.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'    # general shape of the output
#'    corrdat <- tidy_corrm(drosera)
#'    head(corrdat)
#'
#'    # relabeling variables
#'    corrdat1 <- tidy_corrm(drosera,
#'      labels = c("Some", "very", "nice", "labels"))
#'    head(corrdat1)
#'
#'    # use of mutates argument
#'    corrdat2 <- tidy_corrm(
#'      drosera,
#'      mutates = quos(
#'        organ = ifelse(substr(var_x, 1, 1) == "p", "petiole", "leaf"),
#'        dimension = ifelse(grepl("width", var_x), "width", "length")
#'        )
#'     )
#'    head(corrdat2)
#'  }
#' }
#' @seealso [ggcorrm()], [corrmorant()]
#'
#' @rdname tidy_corrm
#' @export
#' @importFrom dplyr mutate_if mutate group_nest select arrange group_by ungroup
#'   case_when group_vars
#' @importFrom stats sd
#' @importFrom tidyr gather expand_grid unnest
tidy_corrm <- function(data, ... ) {
  UseMethod("tidy_corrm", data)
}


# tidy_corrm default method ---------------------------------------------------
#' @export
#' @rdname tidy_corrm
tidy_corrm.default <- function(data,
                               labels = NULL,
                               rescale = c( "as_is", "by_sd", "by_range"),
                               corr_method = c("pearson", "kendall", "spearman"),
                               corr_group = NULL,
                               mutates = NULL,
                               ...) { # for consistency with the generic, currently unused
  # control class of data
  if (!(inherits(data, "data.frame") | is.matrix(data))) {
    stop("data must be a data.frame or matrix.")
  }

  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!rlang::is_quosures(mutates))
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }

  # match arguments
  rescale     <- rlang::arg_match(rescale)
  corr_method <- rlang::arg_match(corr_method)

  # catch grouping variable (if not called via ggcorrm)
  if(!rlang::is_quosure(corr_group)) corr_group <- rlang::enquo(corr_group)

  # compute tidy output
  TidyCorrm$compute(
    data = data,
    arg = list(labels = labels,
               rescale = rescale,
               corr_method = corr_method,
               corr_group = corr_group,
               mutates = mutates)
  )
}

# TidyCorrm ggproto object ----------------------------------------------------
#' @export
#' @rdname corrmorant_ggproto
TidyCorrm <- ggproto(
  "TidyCorrm", NULL,
  # ...Function for computing reshaped variables -----
  compute = function(self, data, arg) {
    # call preprocessing function
    data <- self$preprocess_data(
      data = data, arg = arg
    )

    # call reshaping function
    data_long <- self$reshape_data(
      data = data, arg = arg
    )

    # call postprocessing function
    data_out <- self$postprocess_data(
      data = data_long, arg = arg
    )

    # prepare output
    structure(
      data_out,
      class = c("tidy_corrm", class(data_out)),
      corr_method = arg$corr_method,
      corr_group  = arg$corr_group
    )
  },
  # ...Preprocessing function ----
  preprocess_data = function(self, data, arg){
    # if data is a matrix, convert to data.frame
    if (is.matrix(data)) data <- as.data.frame(data)

    # if necessary, remove groups and issue a message
    if (dplyr::is_grouped_df(data)){
      message("Grouping variables are ignored in tidy_corrm().\n",
              "The following grouping variables have been dropped: ",
              paste(dplyr::group_vars(data), collapse = ", "), "\n")
      data <- dplyr::ungroup(data)
    }

    # replace infinite values and NaN values by NA values to avoid problems
    # during transformation and when calculating correlations
    data[is.infinite(data)] <- NA
    data[is.nan(data)] <- NA
    # return preprocessed data
    data
  },
  # ...Reshaping function -----
  reshape_data = function(self, data, arg){
    # get column numbers of numeric variables
    numerics <- which(sapply(data, is.numeric))

    # if labels were specified, check if they have the right form and rename
    if(!is.null(arg$labels)){
      old <- names(data)[numerics]
      if (is.function(arg$labels)){
         new <- arg$labels(old)
         if (!(is.character(new) & length(new) == length(old)))
           stop("Labelling function must return a character vector with a length equal to the number of numeric columns.")
      } else {
      if (length(arg$labels) != length(numerics))
        stop("Number of labels must be equal to the number of numeric columns.")
        new <- arg$labels
        }
      # print changes of column names
      message("The following column names were replaced:\n",
              paste(old, new, sep = "\t->\t", collapse = "\n"),
              "\n",  sep = "")

      # reset names of numeric columns
      names(data)[numerics] <- new
    }

    # print message if there are more than 10 numeric variables
    if (length(numerics) > 9){
      message("Correlation matrix contains ", length(numerics),
              " numeric variables:\n",
              "Plotting may take very long.\n")
    }

    # print message if there are highly skewed variables
    skews <- apply(data[, numerics], 2, skew)
    if (any(abs(skews) > 1)){
      message("Some variables are highly skewed (abs(skew) > 1).\n",
              "Consider transformation for better display.")
    }

    # perform scale transformation if specified
    if (arg$rescale == "by_sd") {
      data <- dplyr::mutate_if(data, is.numeric,
                               function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE))
    }
    if (arg$rescale == "by_range") {
      data <- dplyr::mutate_if(data, is.numeric,
                               function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
    }

    # group by correlation grouping variable
    data <- dplyr::mutate(data, corr_group = !!arg$corr_group)
    # if !!corr_group evaluated to NULL set uniform group
    if (!rlang::has_name(data, "corr_group")) data$corr_group <- 1

    # reshape dataset to long format
    longtab <- tidyr::gather(data, key = "var", value = "x", numerics) %>%
      # control order of levels
      dplyr::mutate(var = factor(var, levels = names(data)[numerics],
                                 ordered = TRUE)) %>%
      # group by variable and nest
      dplyr::group_nest(var) %>%
      # add reduced nested dataset for y
      dplyr::mutate(dat_y = lapply(data,
                                   function(dat) dplyr::select(dat, y = x)
      )
      ) %>%
      # assure correct order (important for correct subsetting)
      dplyr::arrange(var)

    # prepare plotting output (all x and y and identifiers for everything else)
    # get all combinations of x and y values
    out <- tidyr::expand_grid(var_x = longtab$var, var_y = longtab$var) %>%
      # create columns for the corresponding data
      dplyr::mutate(dat_x = longtab$data[as.numeric(var_x)],
                    dat_y = longtab$dat_y[as.numeric(var_y)]) %>%
      # unnest data columns
      tidyr::unnest(cols = c(dat_x, dat_y)) %>%
      # get column with correlations (for use in plots)
      dplyr::group_by(var_x, var_y, corr_group) %>%
      dplyr::mutate(.corr = cor(x, y, method = arg$corr_method,
                                use = "pairwise.complete.obs")
      ) %>%
      dplyr::ungroup() %>%
      # get indicator for position
      dplyr::mutate(pos = dplyr::case_when(var_x >  var_y ~ "utri",
                                           var_x <  var_y ~ "lotri",
                                           var_x == var_y ~ "dia")) %>%
      dplyr::select(var_x, var_y, x, y, pos, .corr, corr_group,
                    tidyr::everything())

    # return reshaped output
    out
  },
  # ...Post-processing function -----
  postprocess_data = function(self, data, arg){
    # perform post-processing mutates if necessary
    if(!is.null(arg$mutates)) data <- dplyr::mutate(data, !!!arg$mutates)
    # return prost-processed data
    data
  }
)
