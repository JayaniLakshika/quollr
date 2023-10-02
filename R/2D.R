#' Generate the Hexagonal Bins
#'
#' This function generates hexagonal bins based on the provided data and returns an S4 object of class "hexbin".
#'
#' @param .data The input data frame containing the original dataset.
#' @param nldr_df The non-linear dimensionality reduction data frame containing the 2D coordinates.
#' @param embedding_1 The name of the column in the non-linear dimensionality reduction data frame containing the first embedding coordinate.
#' @param embedding_2 The name of the column in the non-linear dimensionality reduction data frame containing the second embedding coordinate.
#' @param num_bins The number of hexagonal bins to generate. Default is 30.
#' @param shape_val The shape parameter for the hexagonal bins. Default is 1.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{df_new}{A data frame with the merged data and added hexbin IDs.}
#'   \item{hb}{An S4 object of class "hexbin" representing the generated hexagonal bins.}
#' }
#'
#' @importFrom dplyr mutate inner_join select starts_with
#' @importFrom hexbin hexbin
#' @importFrom tidyr gather
#'
#' @examples
#' data <- tibble::tribble(
#'   ~ID, ~x, ~y, ~pc1, ~pc2,
#'   1, 0.2, 0.5, 0.1, 0.3,
#'   2, 0.3, 0.4, 0.2, 0.2,
#'   3, 0.1, 0.7, 0.3, 0.1
#' )
#' nldr <- tibble::tribble(
#'   ~ID, ~embedding1, ~embedding2,
#'   1, 0.6, 0.9,
#'   2, 0.7, 0.8,
#'   3, 0.5, 0.7
#' )
#' create_hexbin_df(data, nldr, "embedding1", "embedding2")
#'
#' @export
create_hexbin_df <- function(.data, nldr_df, embedding_1, embedding_2, num_bins = 30,
                          shape_val = 1) {

  ### Merge tSNE dataset in 2D with original dataset which contains
  ### 4D coordinates
  .data <- .data |>
    dplyr::mutate(ID = dplyr::row_number())

  df_new <- .data |>
    dplyr::inner_join(nldr_df, by = "ID")

  ### Fit hexbins and store hexbin IDs
  hb <- hexbin::hexbin(nldr_df |>
                         dplyr::pull({
                           {
                             embedding_1
                           }
                         }), nldr_df |>
                         dplyr::pull({
                           {
                             embedding_2
                           }
                         }), xbins = num_bins, IDs = TRUE, shape = shape_val)
  ### Add hexbin Ids as a column to the original dataset

  df_new <- df_new |>
    dplyr::mutate(hb_id = hb@cID)

  return(list(df_new = df_new, hb = hb))
}


#' Extract Hexagonal Bin Centroids
#'
#' This function computes the x and y coordinates of the centroids from the provided hexagon cell IDs and returns a data frame with the extracted centroid information.
#'
#' @param .data The original data frame containing the data points.
#' @param hb An S4 object of class "hexbin" representing the hexagonal bins.
#'
#' @return A data frame containing the extracted centroid information, including the x and y coordinates of each centroid and the corresponding hexbin ID and cell count.
#'
#' @examples
#' df <- tibble::tibble(pc1 = c(0.1, 0.2, 0.3), pc2 = c(0.3, 0.2, 0.1), hb_id = c(1, 2, 3))
#' hb <- hexbin::hexbin(df$pc1, df$pc2, xbins = 30, IDs = TRUE, shape = 1)
#' extract_hexbin_centroids(df, hb)
#'
#' @importFrom dplyr inner_join mutate rename
#' @importFrom hexbin hcell2xy
#' @export
extract_hexbin_centroids <- function(.data, hb) {
  # Computes x and y coordinates from hexagon cell IDs
  xy <- hexbin::hcell2xy(hb)

  d_cell <- tibble::tibble(x_val_center = xy$x, y_val_center = xy$y)

  # Data of each cell (bin) which contains ID as hex_bin ID
  df_cell_data1 <- tibble::tibble(ID = hb@cell, Cell_count = hb@count)

  df_cell_data <- dplyr::bind_cols(df_cell_data1, d_cell)

  df_cell_data <- df_cell_data |>
    dplyr::rename(hb_id = "ID")

  # Merge hexbin data with the original data frame
  df_b_with_center_data <- .data |>
    dplyr::inner_join(df_cell_data, by = "hb_id")

  df_b_with_center_data <- df_b_with_center_data |>
    dplyr::mutate(ID = dplyr::row_number())

  return(df_b_with_center_data)
}


#' Triangulate Bin Centroids
#'
#' This function triangulates the bin centroids using the x and y coordinates provided in the input data frame and returns the triangular object.
#'
#' @param .data The data frame containing the bin centroids.
#' @param x The name of the column that contains x coordinates of centroids.
#' @param y The name of the column that contains y coordinates of centroids.
#'
#' @return A triangular object representing the triangulated bin centroids.
#'
#' @examples
#' df <- tibble::tibble(x_val_center = rnorm(100), y_val_center = rnorm(100))
#' triangulate_bin_centroids(df, x = x_val_center, y = y_val_center)
#'
#' @importFrom dplyr pull
#' @importFrom tripack tri.mesh
#' @export
triangulate_bin_centroids <- function(.data, x, y){
  tr1 <- tripack::tri.mesh(.data |> dplyr::pull({{ x }}), .data |> dplyr::pull({{ y }}))
  return(tr1)
}


#' Generate Edge Information
#'
#' This function generates edge information from a given triangular object, including the coordinates of the vertices and the from-to relationships between the vertices.
#'
#' @param triangular_object The triangular object from which to generate edge information.
#'
#' @return A data frame containing the edge information, including the from-to relationships and the corresponding x and y coordinates.
#'
#' @examples
#' tr_obj <- tripack::tri.mesh(x = c(1, 2, 3), y = c(4, 5, 6))
#' generate_edge_info(tr_obj)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter row_number pull nth bind_rows
#' @importFrom tripack triangles
#' @export
generate_edge_info <- function(triangular_object) {
  # Create a data frame with x and y coordinate values from the triangular object
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)
  tr_df <- tr_df |> dplyr::mutate(ID = dplyr::row_number())  # Add ID numbers for joining with from and to points in tr_arcs

  # Extract the triangles from the triangular object
  trang <- tripack::triangles(triangular_object)
  trang <- tibble::as_tibble(trang)

  # Create data frames with from-to edges
  tr_arcs_df1 <- tibble::tibble(from = trang$node1, to = trang$node2)
  tr_arcs_df2 <- tibble::tibble(from = trang$node1, to = trang$node3)
  tr_arcs_df3 <- tibble::tibble(from = trang$node2, to = trang$node3)
  tr_arcs_df <- dplyr::bind_rows(tr_arcs_df1, tr_arcs_df2, tr_arcs_df3)

  # Create an empty data frame to store the edge information
  vec <- stats::setNames(rep("", 6), c("from", "to", "x_from", "y_from", "x_to", "y_to"))  # Define column names
  tr_from_to_df_coord <- dplyr::bind_rows(vec)[0, ]
  tr_from_to_df_coord <- tr_from_to_df_coord |> dplyr::mutate_if(is.character, as.numeric)

  # Generate the edge information
  for (i in 1:NROW(tr_arcs_df)) {
    from_row <- tr_df |> dplyr::filter(dplyr::row_number() == (tr_arcs_df |> dplyr::pull(from) |> dplyr::nth(i)))
    to_row <- tr_df |> dplyr::filter(dplyr::row_number() == (tr_arcs_df |> dplyr::pull(to) |> dplyr::nth(i)))
    tr_from_to_df_coord <- tr_from_to_df_coord |> tibble::add_row(
      from = from_row |> dplyr::pull(ID),
      to = to_row |> dplyr::pull(ID),
      x_from = from_row |> dplyr::pull(x),
      y_from = from_row |> dplyr::pull(y),
      x_to = to_row |> dplyr::pull(x),
      y_to = to_row |> dplyr::pull(y)
    )
  }

  return(tr_from_to_df_coord)
}

#' Calculate 2D Distances
#'
#' This function calculates the 2D distances between points in a given data frame.
#'
#' @param .data The data frame containing the points for which to calculate distances.
#'
#' @return A data frame with the from-to relationships and the corresponding 2D distances.
#'
#' @examples
#' df <- tibble::tribble(
#'   ~from, ~to, ~x_from, ~y_from, ~x_to, ~y_to,
#'   1, 2, 6, 0, 3, 4,
#'   1, 3, 7, 0, 5, 12,
#'   2, 3, 3, 4, 5, 12
#' )
#' cal_2D_dist(df)
#'
#' @importFrom dplyr select
#' @export
cal_2D_dist <- function(.data) {
  # Calculate the 2D distances
  .data$distance <- lapply(seq(nrow(.data)), function(x) {
    start <- unlist(.data[x, c("x_from", "y_from")])
    end <- unlist(.data[x, c("x_to", "y_to")])
    sqrt(sum((start - end)^2))
  })

  # Create a data frame with the from-to relationships and distances
  distance_df <- .data |> dplyr::select("from", "to", "distance")

  # Convert the distances to a vector and return the data frame
  distance_df$distance <- unlist(distance_df$distance)
  return(distance_df)
}


#' Color Long Edges
#'
#' This function colors the long edges in a triangular mesh plot based on a benchmark value.
#'
#' @param .data The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges.
#' @param triangular_object The triangular object containing the mesh information.
#' @param distance_col The column name in `.data` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are colored differently.
#'
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual
#' @importFrom stats setNames
#' @importFrom tibble tibble as_tibble
#' @importFrom merge merge
#'
#' @examples
#' df <- tibble::tribble(
#'   ~from, ~to, ~distance,
#'   1, 2, 5,
#'   1, 3, 12.2,
#'   2, 3, 8.25
#' )
#' tr_object <- tripack::tri.mesh(df$from, df$to)
#' colour_long_edges(df, 5, tr_object, "distance")
#'
#' @export
colour_long_edges <- function(.data, benchmark_value, triangular_object, distance_col) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)

  # Generate edge information
  tr_from_to_df_coord <- generate_edge_info(triangular_object)

  # Filter and label small and long edges
  distance_df_small_edges <- .data |>
    dplyr::filter({{ distance_col }} < benchmark_value) |>
    dplyr::mutate(type = "small_edges")

  distance_df_long_edges <- .data |>
    dplyr::filter({{ distance_col }} >= benchmark_value) |>
    dplyr::mutate(type = "long_edges")

  # Combine small and long edges
  distance_edges <- dplyr::bind_rows(distance_df_small_edges, distance_df_long_edges)

  # Merge edge information with distance data
  tr_from_to_df_coord_with_group <- merge(tr_from_to_df_coord, distance_edges, by = c("from", "to"))

  # Create the triangular mesh plot with colored long edges
  tri_mesh_plot <- ggplot2::ggplot(tr_df, aes(x = x, y = y)) +
    ggplot2::geom_segment(
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = type),
      data = tr_from_to_df_coord_with_group
    ) +
    ggplot2::geom_point() +
    ggplot2::coord_equal() +
    ggplot2::scale_colour_manual(values = c("#de2d26", "#636363"))

  return(tri_mesh_plot)
}


remove_long_edges <- function(.data, benchmark_value, triangular_object,
                              distance_col) {
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)

  tr_from_to_df_coord <- generate_edge_info(triangular_object)

  distance_df_small_edges <- .data |>
    dplyr::filter({
      {
        distance_col
      }
    } < benchmark_value)

  tr_from_to_df_coord_with_group <- merge(tr_from_to_df_coord, distance_df_small_edges,
                                          by = c("from", "to"))


  ## To draw the tri.mesh plot using ggplot
  tri_mesh_plot <- ggplot2::ggplot(tr_df, aes(x = x, y = y)) + ggplot2::geom_segment(aes(x = x_from,
                                                                                         y = y_from, xend = x_to, yend = y_to), data = tr_from_to_df_coord_with_group) +
    ggplot2::geom_point(size = 1) + ggplot2::coord_equal() + ggplot2::labs(color=NULL)
  return(tri_mesh_plot)

}


draw_full_hexgrid <- function(.data = data, nldr_df = training_data,
                              embedding_1 = UMAP1, embedding_2 = UMAP2,
                              num_bins = num_bins_x, shape_val = shape_val){

  hb_data <- hexbin::hexbin(x = nldr_df |> pull({{embedding_1}}),
                            y = nldr_df |> pull({{embedding_2}}),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  hexdf_data <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_data)),  hexID = hb_data@cell, counts = hb_data@count/max(hb_data@count))

  #hexdf_data <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_data)),  hexID = hb_data@cell, counts = hb_data@count)

  hex_grid <- expand.grid(nldr_df |> pull({{embedding_1}}), nldr_df |> pull({{embedding_2}}))

  hex_grid_all <- expand.grid(min(nldr_df |> pull({{embedding_1}})): max(nldr_df |> pull({{embedding_1}})),
                              min(nldr_df |> pull({{embedding_2}})): max(nldr_df |> pull({{embedding_2}})))

  hex_grid <- bind_rows(hex_grid, hex_grid_all) %>%
    distinct()

  hb <- hexbin::hexbin(x = hex_grid |> pull(Var1),
                       y = hex_grid |> pull(Var2),
                       xbins = num_bins, IDs = TRUE,
                       shape = shape_val)

  #hexdf <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb)),  hexID = hb@cell, counts = hb@count/max(hb@count))
  # hexdf <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb)),  hexID = hb@cell, counts = hb@count)
  #
  # hexdf <- hexdf %>%
  #   mutate(counts = ifelse((hexID %in% (hexdf_data |> pull(hexID))),counts, NA))

  hexdf <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb)),  hexID = hb@cell)
  hexdf <- full_join(hexdf, hexdf_data, by = c("hexID" = "hexID", "x" = "x", "y" = "y"))

  embedding_hb <- create_hexbin(.data = .data, nldr_df = nldr_df,
                                embedding_1 = {{embedding_1}}, embedding_2 = {{embedding_2}},
                                num_bins = num_bins, shape_val = shape_val)$df_new

  full_grid <- full_join(hexdf, embedding_hb, by = c("hexID" = "hb_id"))

  ggplot(full_grid, aes(x = x, y = y, fill = counts, hexID = hexID)) +
    geom_hex(stat = "identity", color = "#969696") +
    #geom_label(size = 1.8) +
    #geom_point(aes(x = tsne1, y = tsne2), na.rm = TRUE) +
    scale_fill_viridis_c(na.value = "#ffffff") +
    #ggtitle(paste0("A = ", 1 , ", b = (", hb_data@dimen[2], ", ", hb_data@dimen[1], ")")) +
    theme_linedraw() +
    #coord_equal() +
    theme(legend.position = "none", plot.title = element_text(size = 5, hjust = 0.5, vjust = -0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()#change legend key width
    )

}


plot_dist <- function(distance_df){
  distance_df$group <- "1"
  dist_plot <- ggplot(distance_df, aes(x = group, y = distance)) +
    geom_quasirandom()+
    ylim(0, max(unlist(distance_df$distance))+ 0.5) + coord_flip()
  return(dist_plot)
}

cal_2D_dist_umap <- function(.data){

  .data$distance <- lapply(seq(nrow(.data)), function(x) {
    start <- unlist(.data[x, c("avg_umap1","avg_umap2")])
    end <- unlist(.data[x, c("UMAP1","UMAP2")])
    sqrt(sum((start - end)^2))})

  distance_df_cal <- .data %>%
    dplyr::select("hb_id", "avg_umap1","avg_umap2", "UMAP1","UMAP2", "distance")

  distance_df_cal$weight <- 1/(unlist(distance_df_cal$distance) + 0.05)
  return(distance_df_cal)
}
