#' Create a hexgrid plot
#'
#' @param mapping Aesthetic mappings for the plot.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied.
#' @param position The position adjustment to be applied.
#' @param show.legend Whether to show the legend for this layer.
#' @param na.rm Whether to remove missing values.
#' @param inherit.aes Whether to inherit aesthetics from the plot or the layer.
#' @param ... Additional arguments to be passed to the `layer` function.
#'
#' @return A `ggplot2` layer object.
#'
#' @examples
#' df_bin_centroids <- scurve_model_obj$model_2d |> dplyr::filter(n_h > 10)
#' ggplot2::ggplot() +
#' geom_hexgrid(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y))
#'
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 aes
#' @export
geom_hexgrid <- function(mapping = NULL, data = NULL, stat = "hexgrid",
                         position = "identity", show.legend = NA, na.rm = FALSE, inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHexgrid,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

#' GeomHexgrid: A Custom ggplot2 Geom for Hexagonal Grid
#'
#' This function defines a custom ggplot2 Geom, GeomHexgrid, for rendering hexagonal grid.
#'
#' @format A ggproto object
#'
GeomHexgrid <- ggplot2::ggproto("GeomHexgrid",
                                ggplot2::Geom,
                                required_aes = c("x", "y", "xend", "yend"),
                                default_aes = ggplot2::aes(
                                  linetype = 1,
                                  linewidth = 0.5,
                                  alpha = NA,
                                  colour = "#252525"
                                ),
                                draw_key = ggplot2::draw_key_point,
                                draw_panel = function(data, panel_scales, coord) {

                                  hexgrid <- tibble::tibble(
                                    x = data$x,
                                    xend = data$xend,
                                    y = data$y,
                                    yend = data$yend,
                                    PANEL = data$PANEL,
                                    group = data$group,
                                    linetype = data$linetype,
                                    linewidth = data$linewidth,
                                    alpha = data$alpha,
                                    colour = data$colour
                                  )

                                  ggplot2:::ggname(
                                    "geom_hexgrid",
                                    ggplot2::GeomSegment$draw_panel(hexgrid, panel_scales, coord)
                                  )
                                }
)
