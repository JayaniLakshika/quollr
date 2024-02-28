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
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
#' y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA, col_start = "UMAP")
#' all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
#' counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' ggplot2::ggplot() +
#' geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y))
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
GeomTrimesh <- ggplot2::ggproto("GeomTrimesh",
                                ggplot2::Geom,
                                required_aes = c("x", "y", "xend", "yend"),
                                default_aes = ggplot2::aes(
                                  shape = 19,
                                  linetype = 1,
                                  linewidth = 0.5,
                                  size = 0.5,
                                  alpha = NA,
                                  colour = "#33a02c"
                                ),
                                draw_key = ggplot2::draw_key_point,
                                draw_panel = function(data, panel_scales, coord) {

                                  point_info <- tibble::tibble(
                                    x = c(data$x, data$xend),
                                    y = c(data$y, data$yend)
                                  ) |>
                                    dplyr::distinct()

                                  vertices <- tibble::tibble(
                                    x = point_info$x,
                                    y = point_info$y,
                                    colour = rep("#33a02c", nrow(point_info)),
                                    shape = rep(data$shape[1], nrow(point_info)),
                                    size = rep(2, nrow(point_info)),
                                    fill = rep("#33a02c", nrow(point_info)),
                                    alpha = rep(data$alpha[1], nrow(point_info)),
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
                                      ggplot2::GeomSegment$draw_panel(trimesh, panel_scales, coord),
                                      ggplot2::GeomPoint$draw_panel(vertices, panel_scales, coord)
                                    )
                                  )
                                }
)
