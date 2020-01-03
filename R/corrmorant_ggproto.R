#' @title ggproto classes for corrmorant
#' @description Class definitions for the new \code{ggproto} classes defined in
#'     the \code{corrmorant} package.
#' @section New stats:
#' \itemize{
#'   \item \code{StatCortext}: create and position text labels for correlation strength
#'      (see \code{\link{stat_cortext}} for details).
#'   \item \code{StatDiaNames}: create and position text labels for variable names in the
#'      diagonal facets of corrmoran plots (see \code{\link{dia_names}} for details).
#'   \item \code{StatDiaDensity}: create and position density plots of variables in the
#'      diagonal facets of corrmoran plots (see \code{\link{dia_density}} for details).
#'   \item \code{StatDiaBin}: create and position plots of binned data (histograms or
#'     frequency polygons) of variables in the diagonal facets of corrmoran plots
#'     (see \code{\link{dia_histogram}} and  \code{\link{dia_freqpoly}}
#'     for details).
#'   }
#' @seealso
#'  \code{\link[ggplot2]{ggplot2-ggproto}},
#'  \code{\link{stat_cortext}},
#'  \code{\link{dia_names}},
#'  \code{\link{dia_density}},
#'  \code{\link{dia_histogram}},
#'  \code{\link{dia_freqpoly}}
#' @name corrmorant_ggproto
NULL
