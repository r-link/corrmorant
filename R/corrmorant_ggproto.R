#'@title ggproto classes for corrmorant
#'
#'@description Class definitions for the new `ggproto` classes defined in the
#'  `corrmorant` package.
#'
#'@section New stats: * `TidyCorrm`: prototype for [tidy_corrm()] constructor
#'  methods.
#'
#'  * `StatCorrtext`: create and position text labels for correlation strength
#'  (see [stat_corrtext] for details).
#'
#'  * `StatCorrtext`: create and position text labels for the outcome of user
#'  defined functions (see [stat_funtext] for details).
#'
#'  * `StatHeatmap`: create correlation heat maps (see [stat_heatmap] for
#'  details).
#'
#'  * `StatHeatcircle`: create circles indicating correlation strength (see
#'  [stat_heatcircle] for details).
#'
#'  * `StatDiaNames`: create and position text labels for variable names in the
#'  diagonal facets of `ggcorrm` plots (see [stat_dia_names] for details).
#'
#'  * `StatDiaBin`: create and position plots of binned data (histograms or
#'  frequency polygons) of variables in the diagonal facets of `ggcorrm` plots
#'  (see [stat_dia_bin] for details).
#'
#'
#'@section New geoms: * `GeomDiaDensity`: create and position density plots in
#'  the diagonal facets of `ggcorrm` plots (see [geom_dia_density] for details).
#'
#'  * `GeomDiaHistogram`: create and position histogramsin the diagonal facets
#'  of `ggcorrm` plots (see [geom_dia_density] for details).
#'
#'  * `GeomDiaFreqpoly`: create and position frequency polygons in the diagonal
#'  facets of `ggcorrm` plots (see [geom_dia_density] for details).
#'
#'  * `GeomRelpoint`: scatter plots with relative positions (see [geom_relpoint]
#'  for details)-
#'
#'  * `GeomReltext`: text labels with relative positions (see [geom_reltext] for
#'  details).
#'
#'@seealso * [ggplot2-ggproto] for a description of the `ggproto` object
#'orientation system, * [tidy_corrm] for structured tidy correlation plot
#'datasets, * [add_corrtext] for correlation text labels, * [add_funtext] for
#'user-defined text labels, * [add_heatmap] for correlation heatmaps, *
#'[add_heatcircle] for circles witch correlation-dependent size and color, *
#'[dia_names] for variable names in the plot diagonal, * [dia_density] for
#'density plots in the plot diagonal, * [dia_histogram] for histograms and
#'frequency polygons in the plot diagonal
#'
#'@name corrmorant_ggproto
NULL
