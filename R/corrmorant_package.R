#' @title corrmorant - Flexible correlation matrices based on ggplot2
#'
#'@description
#'  \if{html}{\figure{logo.png}{options: align='right' alt='logo' width=250}}
#'  corrmorant extends ggplot2 by an automated framework for plots of
#'  correlation matrices that can be easily modified via regular ggplot2 syntax.
#'  In addition, it provides a large set of visualization tools for exploratory
#'  data analysis based on correlation matrices.
#'
#'@section Author:
#'
#'  **Roman M. Link** (Maintainer, \email{roman.link@plant-ecology.de}).
#'
#'@section Where to start:
#'
#'  * [corrmorant()] - a simple, stripped down function for fast visualization
#'  of correlation matrices.
#'  * [ggcorrm()] - the workhorse function for all corrmorant plots.
#'  * [tidy_corrm()] - prepare datasets for plotting in corrmorant.
#'  * [corrmorant_selectors] - functions that modify regular `ggplot2` layers to
#'  direct them to the right positions in `ggcorrm()` plots: `dia()`, `lotri()`
#'  and `utri()`.
#'
#'@section Special corrmorant layers:
#'
#'  **Modifying the plot diagonal**
#'  * [dia_names()] - add text labels for variable names.
#'  * [dia_density()] - add density plots.
#'  * [dia_histogram()] - add histograms.
#'  * [dia_freqpoly()] - add frequency polygons.
#'  * [dia()] - add arbitrary ggplot layers to the diagonal.
#'
#'  **Modifying the lower triangle**
#'  * [lotri_corrtext()] - add text labels for correlation strength.
#'  * [lotri_funtext()] - add text labels for user-specified functions.
#'  * [lotri_heatmap()] - add correlation heatmaps.
#'  * [lotri_heatpoint()] - add symbols whose size and color indicates correlation strength.
#'  * [lotri_heatcircle()] - add circles whose area scales with correlation strength.
#'  * [lotri()] - add arbitrary ggplot layers to the lower triangle.
#'
#'  **Modifying the upper triangle**
#'  * [utri_corrtext()] - add text labels for correlation strength.
#'  * [utri_funtext()] - add text labels for user-specified functions.
#'  * [utri_heatmap()] - add correlation heatmaps.
#'  * [utri_heatpoint()] - add symbols whose size and color indicates correlation strength.
#'  * [utri_heatcircle()] - add circles whose area scales with correlation strength.
#'  * [utri()] - add arbitrary ggplot layers to the upper triangle.
#'
#'
#' @docType package
#'@name corrmorant_package
NULL


# import functions from other packages ----------------------------------------
# entire ggplot2 package
#' @import ggplot2

# magrittr style pipelines
#' @importFrom magrittr `%>%`
NULL

# rlang quoting functions
#' @export
#' @importFrom rlang quo quos `%||%`
NULL

# rlang::.data to avoid problems with notes about  "no visible binding for global
# variable"
#' @importFrom rlang .data
NULL

# getFromNamespace for internal ggplot2 functions
#' @importFrom stats density
NULL

# getFromNamespace for internal ggplot2 functions
#' @importFrom utils getFromNamespace
NULL

# gList from grid for custom geoms
#' @importFrom grid gList
NULL

# standard build method for ggplot objects
#' @keywords internal
ggplot_build.ggplot <- utils::getFromNamespace("ggplot_build.ggplot", "ggplot2")

# construct labels for aesthetics
#' @keywords internal
make_labels <- utils::getFromNamespace("make_labels", "ggplot2")

