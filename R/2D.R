#' Extract hexagonal bin centroids and related information from non-linear dimensionality reduction data.
#'
#' @param nldr_df Non-linear dimensionality reduction data frame containing 2D coordinates.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first embedding.
#' @param y The name of the column that contains second embedding.
#'
#' @return A list containing the hexagonal bin centroids data frame and the hexbin object.
#' @export
#'
#' @importFrom hexbin hexbin
#' @importFrom dplyr pull
#' @importFrom tibble as_tibble
#' @importFrom utils globalVariables
#'
#' @examples
#' # Example usage of extract_hexbin_centroids function
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' result <- extract_hexbin_centroids(nldr_df, num_bins, shape_val)
#' hexdf_data <- result$hexdf_data
#' hb_data <- result$hb_data
#' print(hexdf_data)
#' print(hb_data)
#'
#'
#' @rdname extract_hexbin_centroids
extract_hexbin_centroids <- function(nldr_df, num_bins, shape_val = 1, x = UMAP1, y = UMAP2) {

  hb_data <- hexbin::hexbin(x = nldr_df |> dplyr::pull({{ x }}),
                            y = nldr_df |> dplyr::pull({{ y }}),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  hexdf_data <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_data)),  hexID = hb_data@cell, counts = hb_data@count, std_counts = hb_data@count/max(hb_data@count))

  return(list(hexdf_data = hexdf_data, hb_data = hb_data))
}


#' Extract hexagonal bin mean and related information from non-linear dimensionality reduction data.
#'
#' @param nldr_df Non-linear dimensionality reduction data frame containing 2D coordinates.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first embedding.
#' @param y The name of the column that contains second embedding.
#'
#' @return A list containing the hexagonal bin mean data frame and the hexbin object.
#' @export
#'
#' @importFrom hexbin hexbin
#' @importFrom dplyr pull
#' @importFrom tibble as_tibble
#' @importFrom utils globalVariables
#' @importFrom here here
#'
#' @examples
#' # Example usage of extract_hexbin_mean function
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' result <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' hexdf_data <- result$hexdf_data
#' hb_data <- result$hb_data
#' print(hexdf_data)
#' print(hb_data)
#'
#'
#' @rdname extract_hexbin_mean
extract_hexbin_mean <- function(nldr_df, num_bins, shape_val = 1, x = UMAP1, y = UMAP2) {

  hb_data <- hexbin::hexbin(x = nldr_df |> dplyr::pull({{ x }}),
                            y = nldr_df |> dplyr::pull({{ y }}),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  df_cell_data <- nldr_df |>
    dplyr::select(-ID) |>
    dplyr::mutate(hexID = hb_data@cID) |>
    dplyr::group_by(hexID) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  names(df_cell_data) <- c("hexID", "x", "y")


  hexdf_data <- tibble::tibble(df_cell_data, counts = hb_data@count, std_counts = hb_data@count/max(hb_data@count))

  return(list(hexdf_data = hexdf_data, hb_data = hb_data))
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
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' triangulate_bin_centroids(df_bin_centroids, x, y)
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
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' generate_edge_info(triangular_object = tr1_object)
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
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' cal_2D_dist(tr_from_to_df)
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
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2D_dist(tr_from_to_df)
#' colour_long_edges(distance_df, 0.6, tr1_object, "distance")
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

#' Remove Long Edges from a Triangular Mesh Plot
#'
#' This function removes long edges from a triangular mesh plot based on a benchmark value.
#'
#' @param .data The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges. Edges with a distance greater than or equal to this value will be removed.
#' @param triangular_object The triangular object containing the mesh information.
#' @param distance_col The column name in `.data` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are removed.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal labs
#' @importFrom tibble tibble
#' @importFrom stats setNames
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2D_dist(tr_from_to_df)
#' remove_long_edges(distance_df, 0.6, tr1_object, "distance")
#'
#' @export
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


#' Generate Full Grid Centroids
#'
#' This function generates all possible centroids in the full grid based on hexbin data.
#'
#' @param hexdf_data The dataset with hexbin ID and centroid coordinates.
#'
#' @return A tibble containing all possible centroids in the full grid.
#'
#' @importFrom ggplot2 resolution
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' generate_full_grid_centroids(df_bin_centroids)
#'
#' @export
generate_full_grid_centroids <- function(hexdf_data){

  ## Generate initial grid
  full_centroids1 <- tibble::as_tibble(expand.grid(x = seq(min(hexdf_data$x),max(hexdf_data$x), ggplot2::resolution(hexdf_data$x, FALSE) * 2), y = seq(min(hexdf_data$y),max(hexdf_data$y), ggplot2::resolution(hexdf_data$y, FALSE) * 2)))

  ## Generate shifted grid
  full_centroids2 <- tibble::tibble(x = full_centroids1$x + ggplot2::resolution(hexdf_data$x, FALSE), y = full_centroids1$y + ggplot2::resolution(hexdf_data$y, FALSE))
  full_centroids <- dplyr::bind_rows(full_centroids1, full_centroids2)

  return(full_centroids)


}

#' Generate Hexagonal Coordinates
#'
#' This function generates the coordinates of hexagons after passing hexagonal centroids.
#'
#' @param hexdf_data The dataset with all hexbin ID and centroid coordinates.
#'
#' @return A tibble containing the coordinates of hexagons based on hexagonal centroids.
#'
#' @importFrom ggplot2 resolution
#' @importFrom tibble as_tibble
#' @importFrom hexbin hexcoords
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' full_hex_grid(full_centroid_df)
#'
#' @export
full_hex_grid <- function(hexdf_data){

  dx <- ggplot2::resolution(hexdf_data$x, FALSE)
  dy <- ggplot2::resolution(hexdf_data$y, FALSE) / sqrt(3) / 2 * 1.15

  hexC <- hexbin::hexcoords(dx, dy, n = 1)

  n <- length(hexdf_data$x)

  size <- rep(1, length(hexdf_data$x))

  full_hex_coords <- tibble::tibble( x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(hexdf_data$x, each = 6),
                                     y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(hexdf_data$y, each = 6), id = rep(1:length(hexdf_data$x), each = 6))

  return(full_hex_coords)


}


#' Map Hexagon IDs to Centroids in the Full Grid
#'
#' This function generates a data frame with hexagon IDs mapped to their centroids in the full grid.
#'
#' @param full_centroid_df Data frame containing centroid coordinates of the full grid.
#' @param df_bin_centroids Data frame containing hexagon IDs and their centroids.
#'
#' @return A data frame with columns 'x', 'y', 'hexID', and 'counts' representing hexagon centroids and counts.
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' map_hexbin_id(full_centroid_df, df_bin_centroids)
#'
#' @export
map_hexbin_id <- function(full_centroid_df, df_bin_centroids) {

  vec1 <- stats::setNames(rep("", 2), c("x", "y"))  ## Define column names

  full_grid_with_hexbin_id <- dplyr::bind_rows(vec1)[0, ]
  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::mutate_if(is.character, as.numeric)

  for(i in 1:length(sort(unique(full_centroid_df$y)))){

    ## Filter the data set with specific y value
    specific_y_val_df <- full_centroid_df |>
      dplyr::filter(y == sort(unique(full_centroid_df$y))[i])

    ordered_x_df <- specific_y_val_df |>
      dplyr::arrange(x)

    full_grid_with_hexbin_id <- dplyr::bind_rows(full_grid_with_hexbin_id, ordered_x_df)

  }

  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::mutate(hexID = row_number())

  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::rename("c_x" = "x",
                  "c_y" = "y")

  full_grid_with_hexbin_id <- dplyr::full_join(full_grid_with_hexbin_id, df_bin_centroids, by = c("hexID" = "hexID")) |>
    dplyr::select(-c(x, y))

  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::mutate(std_counts = counts/max(counts, na.rm = TRUE))

  return(full_grid_with_hexbin_id)
}




#' Map Polygon ID to Hexagon Coordinates
#'
#' This function maps polygon IDs to the corresponding hexagon coordinates in the full grid.
#'
#' @param full_grid_with_hexbin_id A data frame containing hexagon IDs, centroids, and standardized counts.
#' @param hex_grid A data frame containing all coordinates of hexagons.
#'
#' @return A data frame with hexagon information along with mapped polygon IDs.
#'
#' @importFrom dplyr filter mutate bind_rows
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' hex_grid <- full_hex_grid(full_centroid_df)
#' full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
#' map_polygon_id(full_grid_with_hexbin_id, hex_grid)
#'
#' @export
map_polygon_id <- function(full_grid_with_hexbin_id, hex_grid) {

  full_grid_with_polygon_id <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:length(unique(full_grid_with_hexbin_id$hexID))) {

    full_grid_with_hexbin_id_filtered <- full_grid_with_hexbin_id |>
      dplyr::filter(hexID == unique(full_grid_with_hexbin_id$hexID)[i])

    for (j in 1:length(unique(hex_grid$id))) {

      hex_grid_filtered <- hex_grid |>
        dplyr::filter(id == unique(hex_grid$id)[j])

      status_in_x_range <- dplyr::between(full_grid_with_hexbin_id_filtered$c_x, min(hex_grid_filtered$x), max(hex_grid_filtered$x))
      status_in_y_range <- dplyr::between(full_grid_with_hexbin_id_filtered$c_y, min(hex_grid_filtered$y), max(hex_grid_filtered$y))

      if (any(status_in_x_range) & any(status_in_y_range)) {

        full_grid_with_hexbin_id_filtered <- full_grid_with_hexbin_id_filtered |>
          dplyr::mutate(polygon_id = j)

        full_grid_with_polygon_id <- dplyr::bind_rows(full_grid_with_polygon_id, full_grid_with_hexbin_id_filtered)
      }
    }
  }

  return(full_grid_with_polygon_id)
}


#' Generate Full Grid Information Data Frame
#'
#' This function generates a data frame containing coordinates and identifiers for hexagons,
#' along with additional information like counts and polygon IDs.
#'
#' @param df_bin_centroids A data frame with hexagonal bin centroids.
#'
#' @return A data frame with columns "x", "y", "id", "c_x", "c_y", "hexID", "counts", "std_counts", and "polygon_id".
#'
#' @importFrom dplyr slice arrange bind_cols
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' generate_full_grid_info(df_bin_centroids)
#'
#' @export
generate_full_grid_info <- function(df_bin_centroids) {

  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)

  ## Generate all coordinates of hexagons
  hex_grid <- full_hex_grid(full_centroid_df)

  full_grid_with_polygon_id_df <- map_polygon_id(full_grid_with_hexbin_id, hex_grid)

  full_grid_with_hexbin_id_rep <- full_grid_with_polygon_id_df |>
    dplyr::slice(rep(1:n(), each = 6)) |>
    dplyr::arrange(polygon_id)

  hex_full_count_df <- dplyr::bind_cols(hex_grid, full_grid_with_hexbin_id_rep)

  return(hex_full_count_df)

}


#' Find Points in Hexagonal Bins
#'
#' This function maps points to their corresponding hexagonal bins based on the provided data frames.
#'
#' @param full_grid_with_hexbin_id A data frame with hexagonal bin IDs and coordinates.
#' @param UMAP_data_with_hb_id A data frame with UMAP data and hexagonal bin IDs.
#'
#' @return A data frame with hexagonal bin IDs and the corresponding points.
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' hex_grid <- full_hex_grid(full_centroid_df)
#' full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
#' UMAP_data_with_hb_id <- nldr_df |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' find_pts_in_hexbins(full_grid_with_hexbin_id, UMAP_data_with_hb_id)
#'
#' @export
find_pts_in_hexbins <- function(full_grid_with_hexbin_id, UMAP_data_with_hb_id) {

  pts_df <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:length(UMAP_data_with_hb_id$hb_id)) {

    pts_vec <- UMAP_data_with_hb_id |>
      dplyr::filter(hb_id == UMAP_data_with_hb_id$hb_id[i]) |>
      dplyr::pull(ID)

    hb_pts <- tibble::tibble(hexID = UMAP_data_with_hb_id$hb_id[i], pts = list(pts_vec))

    pts_df <- dplyr::bind_rows(pts_df, hb_pts)

  }

  return(pts_df)

}
