# useful helper functions ----------------------------------------------------

# is.infinite method for data.frames -----------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is.infinite.data.frame
#' @export
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

# is.nan method for data.frames ---------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is.nan.data.frame
#' @export
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

# recale_var() - function for rescaling variables ----------------------------
# rescales arbitrary variables to a fixed proportion of a specified range
# append_x allows to include additional values to calculate the original scale
# (e.g. histograms and densities start at zero by definition, so it has to be
#  included in the range)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param lower PARAM_DESCRIPTION
#' @param upper PARAM_DESCRIPTION
#' @param range PARAM_DESCRIPTION
#' @param append_x PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rescale_var
#' @export
rescale_var <- function(x, lower, upper, range, append_x = NULL){
  # scale into the range from 0 to 1...
    if (!any((x - mean(x)) > 1e-10)  && is.null(append_x)) {
      # ... for vectors with identical values (or of length one)
      x1 <- rep(0.5, length(x))
      } else {
    # ... regular case with appended data
    x1 <- (x - min(c(x, append_x))) / (max(c(x, append_x)) - min(c(x, append_x)))
  }
  # rescale to the right percent range
  x2 <- lower + x1 * (upper - lower)

  # return rescaled variable
  return(range[1] + x2 * diff(range))
}

# skew() - function for skewness (based on e1071::skewness) -------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{na.fail}}
#' @rdname skew
#' @export
#' @importFrom stats na.omit
skew <- function (x) {
  x <- stats::na.omit(x)
  n <- length(x)
  x <- x - mean(x)
  sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
}

# add() - function to add layers to ggplot objects via pipelines --------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .object PARAM_DESCRIPTION
#' @param .fun PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add
#' @export
add <- function(.object, .fun, ...) .object + {{.fun}}(...)


# modify_list() - copied internal function from ggplot2 -----------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param old PARAM_DESCRIPTION
#' @param new PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname modify_list
modify_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}



# is.waive() - copied internal function from ggplot2 --------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is.waive
is.waive <- function (x) {
  inherits(x, "waiver")
}

# fixrange() - add fixed cartesian range --------------------------------------
# # .deprecated, just a placeholder to remember it has to be rewritten
# fixrange <- function(.object, range = c(-2.5, 2.5)){
#   data <- attr(.object, "corrdat")
#   outrange <- attr(data, "range") %>%
#     mutate(min = range[1], max = range[2], center = mean(range), span = abs(diff(range)))
#   attr(data, "range") <- outrange
#   attr(.object, "corrdat") <- data
#   return(.object + coord_cartesian(xlim = range, ylim = range))
# }
