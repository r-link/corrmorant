# is.infinite method for data.frames ------------------------------------------
#' @keywords internal
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

# is.nan method for data.frames -----------------------------------------------
#' @keywords internal
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

# rescale_var() - function for rescaling variables ---------------------------
# rescales arbitrary variables to a fixed proportion of a specified range
# append_x allows to include additional values to calculate the original scale
# (e.g. histograms and densities start at zero by definition, so it has to be
#  included in the range)
#' @keywords internal
rescale_var <- function(x, lower, upper, range, append_x = NULL, na.rm = TRUE){
  # scale into the range from 0 to 1...
    if (!any((x - mean(x, na.rm = na.rm)) > 1e-10)  && is.null(append_x)) {
      # ... for vectors with identical values (or of length one)
      x1 <- rep(0.5, length(x))
      } else {
    # ... regular case with appended data
    x1 <- (x - min(c(x, append_x), na.rm = na.rm)) /
      (max(c(x, append_x) , na.rm = na.rm) - min(c(x, append_x), na.rm = na.rm))
  }
  # rescale to the right percent range
  x2 <- lower + x1 * (upper - lower)

  # return rescaled variable
  return(range[1] + x2 * diff(range, na.rm = na.rm))
}

# skew() - function for skewness (based on e1071::skewness) -------------------
#' @keywords internal
#' @importFrom stats na.omit
skew <- function (x) {
  if (length(unique(x)) == 1) {
    return(0)
    }
  x <- stats::na.omit(x)
  n <- length(x)
  x <- x - mean(x)
  sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
}

# prepare_aes_corrm() - prepare aesthetics for plots in ggcorrm ---------------
#' @keywords internal
#' @importFrom dplyr setdiff intersect
update_aes_corrm <- function (new_aes, standard_aes = aes(x, y)) {
  # warn if aesthetics are specified that are not permitted
  if (any(c("x", "y") %in% names(new_aes))) {
    # get call
    call  <- deparse(sys.calls()[[sys.nframe()-1]][[1]])
    # get problematic aesthetics
    which <- paste(dplyr::intersect(c("x", "y"), names(new_aes)),
                   collapse = " and ")
    # issue warning
    warning("x and y aesthetics are ignored in corrmorant functions.\n",
            which, " in ", call, " overridden by default values.",
            call. = FALSE)
  }
  # update permitted aesthetics
  for (i in dplyr::setdiff(names(new_aes), names(standard_aes))){
    standard_aes[[i]] <- new_aes[[i]]
  }
  # return updated aesthetics
  standard_aes
}


# modify_list() - copied internal function from ggplot2 -----------------------
#' @keywords internal
modify_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

# is.waive() - copied internal function from ggplot2 --------------------------
#' @keywords internal
is.waive <- function (x) {
  inherits(x, "waiver")
}
