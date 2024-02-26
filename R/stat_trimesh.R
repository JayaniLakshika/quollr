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

    ## To extract unique combinations
    tr_arcs_df <- tr_arcs_df |>
      dplyr::mutate(x = pmin(from, to), y = pmax(from, to)) |>
      dplyr::distinct(x, y) |>
      dplyr::rename(c("from" = "x", "to" = "y"))

    ## Map from and to coordinates
    tr_from_to_df_coord <- dplyr::left_join(tr_arcs_df, tr_df, by = c("from" = "ID")) |>
      dplyr::rename(c("x_from" = "x", "y_from" = "y"))
    tr_from_to_df_coord <- dplyr::left_join(tr_from_to_df_coord, tr_df, by = c("to" = "ID"))|>
      dplyr::rename(c("x_to" = "x", "y_to" = "y"))

    trimesh <- tibble::tibble(x = tr_from_to_df_coord$x_from,
                              y = tr_from_to_df_coord$y_from,
                              xend = tr_from_to_df_coord$x_to,
                              yend = tr_from_to_df_coord$y_to,
                              PANEL = as.factor(rep(1, nrow(tr_from_to_df_coord))),
                              group = rep(-1, nrow(tr_from_to_df_coord)),
                              size = rep(0.5, nrow(tr_from_to_df_coord)),
                              linetype = rep(1, nrow(tr_from_to_df_coord)),
                              linewidth = rep(0.5, nrow(tr_from_to_df_coord)),
                              alpha = rep(NA, nrow(tr_from_to_df_coord)),
                              colour = rep("#636363", nrow(tr_from_to_df_coord)))
    trimesh
  },
  required_aes = c("x", "y")
)
