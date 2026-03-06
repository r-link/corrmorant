# corrmorant 0.1.0

## Breaking changes

* **ggplot2 >= 4.0.0 is now required.** The package has been migrated to be
  compatible with ggplot2's S7-based internals.

* `StatHeatcircle` no longer computes circle coordinates directly. Circle
  coordinate generation has moved to `GeomHeatcircle$draw_panel()`, which uses
  the final panel range for correct positioning. Code that relied on the
  intermediate ribbon data from `stat_heatcircle()` will need updating.

## New features

* **New geom-based implementations for heat layers** (#23):
  - `GeomHeatmap` / `geom_heatmap()`: panel-filling rectangles coloured by
    correlation strength, using `coord$backtransform_range()` for correct
    positioning even when other layers modify the plot range.
  - `GeomHeatcircle` / `geom_heatcircle()`: circles whose size scales with
    correlation strength, with coordinate computation in `draw_panel()`.
  - `GeomHeatpoint` / `geom_heatpoint()`: centred points with
    correlation-dependent aesthetics.

* The convenience functions `lotri_heatmap()`, `utri_heatmap()`,
  `lotri_heatcircle()`, `utri_heatcircle()`, `lotri_heatpoint()`, and
  `utri_heatpoint()` now use the new geom-based implementations.

## Bug fixes

* Fixed `guides$setup()` error when building plots with ggplot2 >= 4.0.0 (#29).
  The migration rewrote `ggcorrm()` to use the `ggplot()` constructor, switched
  `ggplot_build.ggcorrm()` to `NextMethod()`, and removed `getFromNamespace()`
  calls.

* Fixed `theme_corrm()` positional argument matching broken by ggplot2 v4's new
  `header_family` parameter in `theme_grey()`.

* Replaced deprecated `..corr..` and `..fun_out..` syntax with `after_stat(corr)`
  and `after_stat(fun_out)`.

* Replaced deprecated `size` aesthetic with `linewidth` in `GeomDiaDensity`,
  `GeomDiaHistogram`, and `GeomDiaFreqpoly` default aesthetics.

## Internal changes

* Removed `getFromNamespace()` calls for `ggplot_build.ggplot` and `make_labels`.
* Removed `MASS` and `utils` from Imports (moved `MASS` to Suggests for vignette
  use only).
* Removed self-qualifying `corrmorant:::rescale_var` references.
* `R (>= 4.1.0)` now required (for native pipe support in dependencies).

## Tests

* Expanded test suite from 98 to 159 tests covering stats, geoms, scales, and
  themes. New test files: `test-stat_heatcircle.R`, `test-stat_heatmap.R`,
  `test-stat_funtext.R`, `test-stat_dia_bin.R`, `test-scales_themes.R`,
  `test-geom_heat.R`.
