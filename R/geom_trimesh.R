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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' ggplot2::ggplot() + geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = x, y = y))
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

#' GeomTrimesh: A Custom ggplot2 Geom for Triangular Meshes
#'
#' This function defines a custom ggplot2 Geom, GeomTrimesh, for rendering triangular meshes.
#'
#' @format A ggproto object
#'
#' @details
#' - \code{required_aes}: The required aesthetics for this geometry are \code{"x"}, \code{"y"}, \code{"xend"}, and \code{"yend"}.
#' - \code{default_aes}: The default aesthetics for this geometry include \code{shape = 19}, \code{linetype = 1}, \code{linewidth = 0.5},
#'   \code{size = 0.5}, \code{alpha = NA}, and \code{colour = "black"}.
#' - \code{draw_key}: The function describing how to draw the key glyph is \code{ggplot2::draw_key_point}.
#' - \code{draw_panel}: The function describing how to draw the panel takes \code{data}, \code{panel_scales}, and \code{coord}.
#'   It creates a tibble of \code{vertices} and a tibble of \code{trimesh}. The final plot is constructed using \code{ggplot2::GeomPoint$draw_panel}
#'   for vertices and \code{ggplot2::GeomSegment$draw_panel} for trimesh.
#'
#' @examples
#' \dontrun{
#'   # Example usage in a ggplot
#'   ggplot(data = my_data, aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_trimesh()
#' }
#'
#' @export
GeomTrimesh <- ggplot2::ggproto("GeomTrimesh",
                                ggplot2::Geom,
                                required_aes = c("x", "y", "xend", "yend"),
                                default_aes = ggplot2::aes(
                                  shape = 19,
                                  linetype = 1,
                                  linewidth = 0.5,
                                  size = 0.5,
                                  alpha = NA,
                                  colour = "black"
                                ),
                                draw_key = ggplot2::draw_key_point,
                                draw_panel = function(data, panel_scales, coord) {

                                  vertices <- tibble::tibble(
                                    x = data$x,
                                    y = data$y,
                                    colour = data$colour,
                                    shape = data$shape,
                                    size = rep(2, nrow(data)),
                                    fill = rep("black", nrow(data)),
                                    alpha = data$alpha,
                                    stroke = 0.5,
                                    stringsAsFactors = FALSE
                                  )

                                  trimesh <- tibble::tibble(
                                    x = data$x,
                                    xend = data$xend,
                                    y = data$y,
                                    yend = data$yend,
                                    PANEL = data$PANEL,
                                    group = data$group,
                                    size = data$size,
                                    linetype = data$linetype,
                                    linewidth = data$linewidth,
                                    alpha = data$alpha,
                                    colour = data$colour
                                  )

                                  ggplot2:::ggname(
                                    "geom_trimesh",
                                    grid::grobTree(
                                      ggplot2::GeomPoint$draw_panel(vertices, panel_scales, coord),
                                      ggplot2::GeomSegment$draw_panel(trimesh, panel_scales, coord)
                                    )
                                  )
                                }
)
