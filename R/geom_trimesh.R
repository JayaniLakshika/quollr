#' Create a trimesh plot
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
#' # Basic usage
#' df <- tibble::tibble(x = stats::rnorm(10), y = stats::rnorm(10))
#' geom_trimesh(data = df, mapping = ggplot2::aes(x = x, y = y))
#'
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 aes
#' @export
geom_trimesh <- function(mapping = NULL, data = NULL, stat = "trimesh",
                         position = "identity", show.legend = NA, na.rm = FALSE, inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomTrimesh,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggproto
#' @importFrom grid grobTree
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr nth
#' @importFrom dplyr row_number
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' @importFrom tripack tri.mesh
#' @importFrom tripack triangles
#' @importFrom tripack tri.mesh
#' @importFrom tripack triangles
#' @importFrom tripack tri.mesh
#' @importFrom tripack triangles
#' @importFrom tibble add_row


#' GeomTrimesh Custom Geom for trimesh plot
#'
GeomTrimesh <- ggplot2::ggproto("GeomTrimesh", ggplot2::Geom, required_aes = c("x", "y", "xend", "yend"),
                                default_aes = ggplot2::aes(shape = 19, linetype = 1, linewidth = 0.5,
                                                           size = 0.5, alpha = NA, colour = "black"),
                                draw_key = ggplot2::draw_key_point,
                                draw_panel = function(data, panel_scales, coord) {

                                  vertices <- tibble::tibble(x = data$x, y = data$y, colour = data$colour,
                                                             shape = data$shape, size = rep(2, nrow(data)), fill = rep("black",
                                                                                                                       nrow(data)), alpha = data$alpha, stroke = 0.5, stringsAsFactors = FALSE)

                                  trimesh <- tibble::tibble(x = data$x, xend = data$xend, y = data$y,
                                                            yend = data$yend, PANEL = data$PANEL, group = data$group, size = data$size,
                                                            linetype = data$linetype, linewidth = data$linewidth, alpha = data$alpha,
                                                            colour = data$colour)

                                  ggplot2:::ggname("geom_trimesh", grid::grobTree(ggplot2::GeomPoint$draw_panel(vertices,
                                                                                                                panel_scales, coord), ggplot2::GeomSegment$draw_panel(trimesh,
                                                                                                                                                                      panel_scales, coord)))
                                })

