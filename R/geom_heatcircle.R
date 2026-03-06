# GeomHeatcircle - ggproto object for geom_heatcircle -------------------------
#' @rdname corrmorant_ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHeatcircle <- ggplot2::ggproto(
  "GeomHeatcircle", GeomRibbon,
  required_aes = c("x", "y", "corr"),
  default_aes = aes(fill = NA, colour = "black", linewidth = 0.3,
                     linetype = 1, alpha = NA),
  extra_params = c("na.rm", "rmin", "rmax", "scale_by"),
  # bypass GeomRibbon$setup_data which requires ymin/ymax aesthetics
  setup_data = function(data, params) data,
  draw_panel = function(self, data, panel_params, coord,
                        rmin = 0.1, rmax = 0.9,
                        scale_by = "area",
                        na.rm = FALSE, flipped_aes = FALSE) {
    # get final panel ranges
    range <- coord$backtransform_range(panel_params)
    range_x <- range$x
    range_y <- range$y

    # get power for scaling
    pow <- switch(scale_by, area = 0.5, radius = 1)

    # validate rmin and rmax
    if (any(c(rmin, rmax) < 0 | c(rmin, rmax) > 1)) {
      stop("rmin and rmax must be between 0 and 1.")
    }
    if (rmin > rmax) {
      stop("rmin larger than rmax.")
    }

    # compute circle coordinates for each group
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      # each group has one corr value (one row from stat)
      corr <- group$corr[1]

      # center of panel
      xc <- mean(range_x)
      yc <- mean(range_y)

      # get rescaled radii
      rx <- rescale_var(abs(corr) ^ pow,
                        lower = 0,
                        upper = diff(range_x) / 2,
                        range = c(rmin, rmax),
                        append_x = c(0, 1))
      ry <- rescale_var(abs(corr) ^ pow,
                        lower = 0,
                        upper = diff(range_y) / 2,
                        range = c(rmin, rmax),
                        append_x = c(0, 1))

      # compute circle outline (100 points each half)
      theta <- seq(0, pi, length.out = 100)
      x_coords    <- xc + rx * cos(theta)
      ymax_coords <- yc + ry * sin(theta)
      ymin_coords <- yc + ry * sin(-theta)

      # build ribbon data for this group
      n <- length(x_coords)
      ribbon_data <- data.frame(
        x = x_coords,
        ymin = ymin_coords,
        ymax = ymax_coords,
        PANEL = rep(group$PANEL[1], n),
        group = rep(group$group[1], n),
        fill = rep(group$fill[1], n),
        colour = rep(group$colour[1], n),
        linewidth = rep(group$linewidth[1], n),
        linetype = rep(group$linetype[1], n),
        alpha = rep(group$alpha[1], n),
        stringsAsFactors = FALSE
      )
      ribbon_data
    })

    # combine all groups
    combined <- do.call(rbind, grobs)

    # delegate to GeomRibbon
    ggplot2::GeomRibbon$draw_panel(data = combined,
                                    panel_params = panel_params,
                                    coord = coord,
                                    na.rm = na.rm)
  },
  draw_key = ggplot2::draw_key_rect
)


# geom_heatcircle() - geom function for GeomHeatcircle ------------------------
#' @title Display circles with correlation-dependent size
#'
#' @description \code{geom_heatcircle()} displays circles whose size and fill
#'   indicate correlation strength, using panel-aware positioning.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_heatcircle
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning. If \code{TRUE}, silently removes missing values.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}()}.
#'
#' @return An object of class \code{Layer}.
#'
#' @details \code{geom_heatcircle()} computes circle coordinates in the
#'   \code{draw_panel()} step using \code{coord$backtransform_range()},
#'   ensuring correct positioning even when other layers modify the plot range.
#'
#' @seealso
#'   \code{\link{stat_heatcircle}()},
#'   \code{\link[ggplot2]{geom_ribbon}()}
#' @rdname geom_heatcircle
#' @export
geom_heatcircle <- function(mapping = NULL, data = NULL,
                            stat = "heatcircle",
                            position = "identity",
                            ...,
                            rmin = 0.1, rmax = 0.9,
                            scale_by = c("area", "radius"),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  scale_by <- rlang::arg_match(scale_by)
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHeatcircle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      rmin = rmin,
      rmax = rmax,
      scale_by = scale_by,
      na.rm = na.rm,
      ...
    )
  )
}
