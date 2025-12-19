#' stat_hexgrid Custom Stat for hexagonal grid plot
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
stat_hexgrid <- function(mapping = NULL, data = NULL, geom = GeomHexgrid$default_aes(),
                         position = "identity", show.legend = NA, outliers = TRUE, inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    stat = StatHexgrid,
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
#' @importFrom ggplot2 GeomSegment
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate rename left_join bind_rows filter arrange first group_by ungroup n
StatHexgrid <- ggplot2::ggproto(
  "StatHexgrid",
  ggplot2::Stat,
  compute_group = function(data, scales, outliers = TRUE) {

    ## Obtain centroid info
    hex_ids <- 1:NROW(data)
    c_x_vec <- data$x
    c_y_vec <- data$y

    ## To find consecutive values

    df_consec <- data |>
      group_by(y) |>
      arrange(x, .by_group = TRUE) |>
      mutate(
        dx = x - lag(x),              # Difference in x between rows
        consec_group = cumsum(is.na(dx) | dx != first(na.omit(dx)))  # New group when jump changes
      )

    df_consec <- df_consec |>
      group_by(y, consec_group) |>
      filter(n() >= 3) |>
      ungroup()

    ## Hexagonal width
    a1 <- sqrt(sum((df_consec[1,1:2] - df_consec[2,1:2])^2))

    ## To compute vertical spacing factor
    vs_factor <- a1/(2 * sqrt(3))

    dx <- a1/2
    dy <- a1/sqrt(3)

    ## Assign coordinates for 6 directions
    x_add_factor <- c(0, -dx, -dx, 0, dx, dx)
    y_add_factor <- c(dy, vs_factor, -vs_factor, -dy, -vs_factor, vs_factor)

    ## Initialize vectors to store hexagonal coordinates
    hex_poly_id <- integer(0)
    x <- numeric(0)
    y <- numeric(0)

    for (hb_id in hex_ids) {

      ## Since each hexagon has 6 coordinates
      hexID_rep <- rep(hex_ids[hb_id], each = 6)
      c_x_rep <- rep(c_x_vec[hb_id], each = 6)
      c_y_rep <- rep(c_y_vec[hb_id], each = 6)

      ## Generate the 6 coordinates
      x_spec <- c_x_rep + x_add_factor
      y_spec <- c_y_rep + y_add_factor

      ## Append to existing vectors
      x <- append(x, x_spec)
      y <- append(y, y_spec)
      hex_poly_id <- append(hex_poly_id, hexID_rep)

    }

    hex_coord_df <- tibble(hex_poly_id = hex_poly_id, x = x, y = y) |>
      mutate(edge_order = rep(1:6, times = length(unique(hex_poly_id))))

    ## To generate the edges
    hexIDs <- unique(hex_coord_df$hex_poly_id)
    vertices <- 1:6
    edges <- tibble(
      from = vertices,
      to = c(tail(vertices, -1), vertices[1])
    )

    # Create coordinated for from and to
    edges_all <- bind_rows(lapply(hexIDs, function(id) mutate(edges, h = id)))

    edges_all <- left_join(edges_all, hex_coord_df, c("h" = "hex_poly_id", "from" = "edge_order")) |>
      rename(c("x_from" = "x",
               "y_from" = "y"))

    edges_all <- left_join(edges_all, hex_coord_df, c("h" = "hex_poly_id", "to" = "edge_order")) |>
      rename(c("x_to" = "x",
               "y_to" = "y"))

    hexgrid <- tibble::tibble(x = edges_all$x_from,
                              y = edges_all$y_from,
                              xend = edges_all$x_to,
                              yend = edges_all$y_to,
                              PANEL = as.factor(rep(1, nrow(edges_all))),
                              group = rep(-1, nrow(edges_all)),
                              linetype = rep(1, nrow(edges_all)),
                              linewidth = rep(0.5, nrow(edges_all)),
                              alpha = rep(NA, nrow(edges_all)),
                              colour = rep("#252525", nrow(edges_all)))
    hexgrid
  },
  required_aes = c("x", "y")

)
