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
#' @importFrom tripack triangles
#' @importFrom tibble add_row

StatTrimesh <- ggplot2::ggproto(
  "StatTrimesh",
  ggplot2::Stat,
  compute_group = function(data, scales, outliers = TRUE) {
    tr1 <- tripack::tri.mesh(data$x, data$y, duplicate = "remove")
    tr_df <- tibble::tibble(x = tr1$x, y = tr1$y)  ## Create a dataframe with tri.mesh x and y coordinate values
    tr_df <- tr_df |>
      dplyr::mutate(ID = dplyr::row_number())  ## To add ID numbers, because to join with from and to points in tri$arcs

    trang <- tripack::triangles(tr1)
    trang <- tibble::as_tibble(trang)

    tr_arcs_df1 <- tibble::tibble(from = trang$node1, to = trang$node2)  ## Create dataframe with from and to edges
    tr_arcs_df2 <- tibble::tibble(from = trang$node1, to = trang$node3)
    tr_arcs_df3 <- tibble::tibble(from = trang$node2, to = trang$node3)
    tr_arcs_df <- dplyr::bind_rows(tr_arcs_df1, tr_arcs_df2, tr_arcs_df3)  ## Create dataframe with from and to edges

    ## To obtain x and values of from to in a dataframe
    vec <- stats::setNames(rep("", 6), c("from", "to", "x_from", "y_from",
                                         "x_to", "y_to"))  ## Define column names
    # Initialize an empty dataframe to store data in a specific
    # format
    tr_from_to_df_coord <- dplyr::bind_rows(vec)[0, ]
    tr_from_to_df_coord <- tr_from_to_df_coord |>
      dplyr::mutate_if(is.character, as.numeric)

    for (i in 1:NROW(tr_arcs_df)) {
      from_row <- tr_df |>
        dplyr::filter(dplyr::row_number() == (tr_arcs_df |>
                                                dplyr::pull(from) |>
                                                dplyr::nth(i)))
      to_row <- tr_df |>
        dplyr::filter(dplyr::row_number() == (tr_arcs_df |>
                                                dplyr::pull(to) |>
                                                dplyr::nth(i)))
      tr_from_to_df_coord <- tr_from_to_df_coord |>
        tibble::add_row(from = from_row |>
                          dplyr::pull(ID),
                        to = to_row |>
                          dplyr::pull(ID),
                        x_from = from_row |>
                          dplyr::pull(x),
                        y_from = from_row |>
                          dplyr::pull(y),
                        x_to = to_row |>
                          dplyr::pull(x),
                        y_to = to_row |>
                          dplyr::pull(y))  ## Add vector as an appending row to the dataframe
    }

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
