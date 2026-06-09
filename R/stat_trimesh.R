#' stat_trimesh Custom Stat for trimesh plot
#'
#' @param mapping Aesthetic mappings for the plot.
#' @param data The data to be plotted.
#' @param geom The geometry to be used in the plot.
#' @param position The position adjustment to be applied.
#' @param show.legend Whether to show the legend for this layer.
#' @param outliers Whether to include outliers.
#' @param inherit.aes Whether to inherit aesthetics from the plot or the layer.
#' @param ... Additional arguments to be passed to the `layer` function.
#'
#' @return A `ggplot2` layer object.
#'
#'
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 Stat
#' @importFrom ggplot2 Geom
#' @export
stat_trimesh <- function(mapping = NULL, data = NULL, geom = GeomTrimesh$default_aes(),
                         position = "identity", show.legend = NA, outliers = TRUE, inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    stat = StatTrimesh,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, ...)
  )
}


#' @importFrom ggplot2 Stat
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 GeomPoint GeomSegment
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
#' @importFrom interp triangles
#' @importFrom tibble add_row

StatTrimesh <- ggplot2::ggproto(
  "StatTrimesh",
  ggplot2::Stat,
  compute_group = function(data, scales, outliers = TRUE) {

    # Create triangular object
    suppressWarnings(triangular_object <- interp::tri.mesh(data$x, data$y))

    # Create a data frame with x and y coordinate values from the triangular object
    tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y,
                            ID = 1:length(triangular_object$x))  # Add ID numbers for joining with from and to points in tr_arcs

    # Extract the triangles from the triangular object
    trang <- interp::triangles(triangular_object)
    trang <- tibble::as_tibble(trang)

    # Create data frames with from-to edges
    tr_arcs_df <- tibble::tibble(from = c(trang$node1, trang$node1, trang$node2),
                                 to = c(trang$node2, trang$node3, trang$node3))

    # Filter edges based on the bin counts of the connected nodes
    edges_all <- tr_arcs_df |>
      left_join(tr_df |> select(ID), by = c("from" = "ID")) |>
      left_join(tr_df |> select(ID), by = c("to" = "ID")) |>
      select(from, to) |>
      mutate(x = pmin(from, to), y = pmax(from, to)) |>
      distinct(x, y) |>
      rename(from = x, to = y)

    # Map from and to coordinates for the filtered edges
    tr_from_to_df_coord <- left_join(edges_all, tr_df, by = c("from" = "ID")) |>
      rename(x_from = x, y_from = y) |>
      left_join(tr_df, by = c("to" = "ID")) |>
      rename(x_to = x, y_to = y) |>
      select(from, to, x_from, y_from, x_to, y_to) # Keep only necessary columns

    edge_data <- calc_2d_dist(trimesh_data = tr_from_to_df_coord, select_vars = c("from", "to", "x_from", "y_from", "x_to", "y_to", "distance"))
    a1 <- min(edge_data$distance)

    edge_data <- edge_data |>
      dplyr::filter(distance <= sqrt(a1^2 + (sqrt(3) * a1/2)^2)) |> # a2 = sqrt(3) * a1/2
      dplyr::select(-distance)

    trimesh <- tibble::tibble(x = edge_data$x_from,
                              y = edge_data$y_from,
                              xend = edge_data$x_to,
                              yend = edge_data$y_to,
                              PANEL = as.factor(rep(1, nrow(edge_data))),
                              group = rep(-1, nrow(edge_data)),
                              size = rep(0.5, nrow(edge_data)),
                              linetype = rep(1, nrow(edge_data)),
                              linewidth = rep(0.5, nrow(edge_data)),
                              alpha = rep(NA, nrow(edge_data)),
                              colour = rep("#636363", nrow(edge_data)))
    trimesh
  },
  required_aes = c("x", "y")
)
