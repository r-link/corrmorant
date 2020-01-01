# tidy_corrm() - data preparation for the ggcorrm plots -----------------------
# TODO: implement reordering as in corrplot (possibly by importing their functions)

# cormoprep() - function that prepares datasets for plotting in ggcorrm() -----
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param labels PARAM_DESCRIPTION, Default: NULL
#' @param rescale PARAM_DESCRIPTION, Default: c("by_sd", "by_range", NULL)
#' @param corr_group PARAM_DESCRIPTION, Default: NULL
#' @param corr_method PARAM_DESCRIPTION, Default: 'pearson'
#' @param mutates PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_nest}},\code{\link[dplyr]{select}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{case_when}}
#'  \code{\link[stats]{sd}}
#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{expand_grid}},\code{\link[tidyr]{nest}}
#'  \code{\link[purrr]{map}}
#' @rdname tidy_corrm
#' @export 
#' @importFrom dplyr mutate_if mutate group_nest select arrange group_by ungroup case_when
#' @importFrom stats sd
#' @importFrom tidyr gather expand_grid unnest
#' @importFrom purrr map
tidy_corrm <- function(data, 
                       labels = NULL, 
                       rescale = c("by_sd", "by_range", NULL), 
                       corr_group = NULL, 
                       corr_method = "pearson", 
                       mutates = NULL) {
  # if post-rescaling transformations were specified, test if they are valid
  if(!is.null(mutates)){
    if (!is_quosures(mutates)) 
      stop("The transformation(s) specified as 'mutates' must be a named list of quosures\n created with quos()")
  }
  
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
                                          var_x == var_y ~ "diag"))
  
  # if specified, perform desired mutations (must be list of quosures)
  if(!is.null(mutates)) out <- dplyr::mutate(out, !!!mutates)
  
  # return output with appended class (used for testing in corrmorant selectors)
  return(structure(out, class = c(class(out), "tidy_corrm")))
}
