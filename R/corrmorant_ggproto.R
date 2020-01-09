#' @title ggproto classes for corrmorant
#' @description Class definitions for the new \code{ggproto} classes defined in
#'     the \code{corrmorant} package.
#' @section New stats:
#' \itemize{
#'   \item \code{StatCorrtext}: create and position text labels for correlation strength
#'      (see \code{\link{stat_corrtext}} for details).
#'   \item \code{StatHeatmap}: create correlation heat maps (see \code{\link{stat_heatmap}}
#'      for details).
#'   \item \code{StatHeatcircle}: create circles indicating correlation strength (see
#'      \code{\link{stat_heatcircle}} for details).
#'   \item \code{StatDiaNames}: create and position text labels for variable names in the
#'      diagonal facets of \code{ggcorrm} plots (see \code{\link{stat_dia_names}} for details).
#'   \item \code{StatDiaDensity}: create and position density plots of variables in the
#'      diagonal facets of \code{ggcorrm} plots (see \code{\link{stat_dia_density}} for details).
#'   \item \code{StatDiaBin}: create and position plots of binned data (histograms or
#'     frequency polygons) of variables in the diagonal facets of \code{ggcorrm} plots
#'     (see \code{\link{stat_dia_bin}} for details).
#'   }
#' @seealso
#'  \code{\link[ggplot2]{ggplot2-ggproto}},
#'  \code{\link{lotri_corrtext}} and \code{\link{utri_corrtext}},
#'  \code{\link{lotri_heatmap}} and \code{\link{utri_heatmap}},
#'  \code{\link{lotri_heatcircle}} and \code{\link{utri_heatcircle}},
#'  \code{\link{dia_names}},
#'  \code{\link{dia_density}},
#'  \code{\link{dia_histogram}}
#' @name corrmorant_ggproto
NULL
