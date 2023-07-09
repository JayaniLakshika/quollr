#'
#' draw_panel_function <- function(data, panel_scales, coord) {
#'
#'
#'   vertices <- tibble::tibble(x = data$x, y = data$y, colour = data$colour,
#'                              shape = data$shape, size = rep(2, nrow(data)), fill = rep("black",
#'                                                                                        nrow(data)), alpha = data$alpha, stroke = 0.5, stringsAsFactors = FALSE)
#'
#'   trimesh <- tibble::tibble(x = data$x, xend = data$xend, y = data$y,
#'                             yend = data$yend, PANEL = data$PANEL, group = data$group, size = data$size,
#'                             linetype = data$linetype, linewidth = data$linewidth, alpha = data$alpha,
#'                             colour = data$colour)
#'
#'   ggplot2:::ggname("geom_trimesh", grid::grobTree(ggplot2::GeomPoint$draw_panel(vertices,
#'                                                                                 panel_scales, coord), ggplot2::GeomSegment$draw_panel(trimesh,
#'                                                                                                                                       panel_scales, coord)))
#' }
#'
#' #' @rdname GeomTrimesh
#' #' @rdname ggplot2-ggproto
#' #' @importFrom ggplot2 ggproto Geom
#'
#' GeomTrimesh <- ggplot2::ggproto("GeomTrimesh", Geom, required_aes = c("x", "y",
#'                                                              "xend", "yend"), default_aes = aes(shape = 19, linetype = 1, linewidth = 0.5,
#'                                                                                                 size = 0.5, alpha = NA, colour = "black"), draw_key = draw_key_point,
#'                        draw_panel = draw_panel_function)
#'
#' #' @rdname geom_trimesh
#' #' @inheritParams layer
#' #' @eval rd_aesthetics("geom", "trimesh")
#' #' @importFrom ggplot2 layer aes
#' geom_trimesh <- function(mapping = NULL, data = NULL, stat = "trimesh",
#'                          position = "identity", show.legend = NA, na.rm = FALSE, inherit.aes = TRUE,
#'                          ...) {
#'   ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomTrimesh,
#'         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'         params = list(na.rm = na.rm, ...))
#' }
