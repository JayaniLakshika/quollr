#' Extract hexagonal bin centroids and related information from non-linear dimensionality reduction data.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' result <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value, x = UMAP1, y = UMAP2)
#' hexdf_data <- result$hexdf_data
#' hb_data <- result$hb_data
#' print(hexdf_data)
#' print(str(hb_data))
#'
#'
#' @rdname extract_hexbin_centroids
extract_hexbin_centroids <- function(nldr_df, num_bins, shape_val = 1, x = UMAP1, y = UMAP2) {

  ## To create the hexbin object
  hb_data <- hexbin::hexbin(x = nldr_df |> dplyr::pull({{ x }}),
                            y = nldr_df |> dplyr::pull({{ y }}),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  ## To create the hexbin centroid info dataset
  hexdf_data <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_data)),  hexID = hb_data@cell, counts = hb_data@count, std_counts = hb_data@count/max(hb_data@count))

  return(list(hexdf_data = hexdf_data, hb_data = hb_data))
}


#' Extract hexagonal bin mean and related information from non-linear dimensionality reduction data.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' result <- extract_hexbin_mean(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value, x = UMAP1, y = UMAP2)
#' hexdf_data <- result$hexdf_data
#' hb_data <- result$hb_data
#' print(hexdf_data)
#' print(str(hb_data))
#'
#'
#' @rdname extract_hexbin_mean
extract_hexbin_mean <- function(nldr_df, num_bins, shape_val = 1, x = UMAP1, y = UMAP2) {

  ## To create the hexbin object
  hb_data <- hexbin::hexbin(x = nldr_df |> dplyr::pull({{ x }}),
                            y = nldr_df |> dplyr::pull({{ y }}),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  ## To compute hexagonal bin means
  df_cell_data <- nldr_df |>
    dplyr::select(-ID) |>
    dplyr::mutate(hexID = hb_data@cID) |>
    dplyr::group_by(hexID) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  ## To rename the columns
  names(df_cell_data) <- c("hexID", "x", "y")

  ## To create the hexbin means info dataset
  hexdf_data <- tibble::tibble(df_cell_data, counts = hb_data@count, std_counts = hb_data@count/max(hb_data@count))

  return(list(hexdf_data = hexdf_data, hb_data = hb_data))
}


#' Triangulate Bin Centroids
#'
#' This function triangulates the bin centroids/ means using the x and y coordinates provided in the input data frame and returns the triangular object.
#'
#' @param .data The data frame containing the bin centroids/ means.
#' @param x The name of the column that contains x coordinates of bin centroids/ means.
#' @param y The name of the column that contains y coordinates of bin centroids/ means.
#'
#' @return A triangular object representing the triangulated bin centroids/ means.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' generate_edge_info(triangular_object = tr1_object)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter row_number pull nth bind_rows distinct
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

  tr_arcs_df <- tr_arcs_df |> ## To remove duplicates
    dplyr::distinct()

  ## To extract unique combinations
  tr_arcs_df <- tr_arcs_df |>
    dplyr::mutate(x = pmin(from, to), y = pmax(from, to)) |>
    dplyr::distinct(x, y) |>
    dplyr::rename(c("from" = "x", "to" = "y"))

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

#' Calculate 2D Distances Between Points
#'
#' This function calculates the 2D distances between pairs of points in a data frame.
#'
#' @param .data A data frame containing columns for the x and y coordinates of start and end points.
#' @param start_x Column name for the x-coordinate of the starting point.
#' @param start_y Column name for the y-coordinate of the starting point.
#' @param end_x Column name for the x-coordinate of the ending point.
#' @param end_y Column name for the y-coordinate of the ending point.
#' @param select_col_vec A character vector specifying the columns to be selected in the resulting data frame.
#'
#' @return A data frame with columns for the starting point, ending point, and calculated distances.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' cal_2d_dist(tr_from_to_df, start_x = "x_from", start_y = "y_from",
#' end_x = "x_to", end_y = "y_to", select_col_vec = c("from", "to", "distance"))
#'
#' @importFrom dplyr select
#'
#' @export
cal_2d_dist <- function(.data, start_x = "x_from", start_y = "y_from", end_x = "x_to",
                        end_y = "y_to", select_col_vec = c("from", "to", "distance")) {
  # Calculate the 2D distances
  .data$distance <- lapply(seq(nrow(.data)), function(x) {
    start <- unlist(.data[x, c(start_x, start_y)])
    end <- unlist(.data[x, c(end_x, end_y)])
    sqrt(sum((start - end)^2))
  })

  # Create a data frame with the from-to relationships and distances
  distance_df <- .data |> dplyr::select(tidyselect::all_of(select_col_vec))

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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df)
#' colour_long_edges(.data = distance_df, benchmark_value = 5.4,
#' triangular_object = tr1_object, distance_col = distance)
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df)
#' remove_long_edges(.data = distance_df, benchmark_value = 5.4,
#' triangular_object = tr1_object, distance_col = distance)
#'
#' @export
remove_long_edges <- function(.data, benchmark_value, triangular_object,
                              distance_col) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)

  # Generate edge information
  tr_from_to_df_coord <- generate_edge_info(triangular_object)

  # Filter small edges
  distance_df_small_edges <- .data |>
    dplyr::filter({
      {
        distance_col
      }
    } < benchmark_value)

  # Merge edge information with distance data
  tr_from_to_df_coord_with_group <- merge(tr_from_to_df_coord, distance_df_small_edges,
                                          by = c("from", "to"))


  ## Create the triangular mesh plot after removing the long edges
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' generate_full_grid_centroids(df_bin_centroids)
#'
#' @export
generate_full_grid_centroids <- function(hexdf_data){

  ## Generate initial grid
  full_centroids1 <- tibble::as_tibble(expand.grid(x = seq(min(hexdf_data$x),max(hexdf_data$x), ggplot2::resolution(hexdf_data$x, FALSE) * 2), y = seq(min(hexdf_data$y),max(hexdf_data$y), ggplot2::resolution(hexdf_data$y, FALSE) * 2)))

  ## Generate shifted grid
  full_centroids2 <- tibble::tibble(x = full_centroids1$x + ggplot2::resolution(hexdf_data$x, FALSE), y = full_centroids1$y + ggplot2::resolution(hexdf_data$y, FALSE))

  ## Combine all
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' full_hex_grid(full_centroid_df)
#'
#' @export
full_hex_grid <- function(hexdf_data){

  ## Compute horizontal width of the hexagon
  dx <- ggplot2::resolution(hexdf_data$x, FALSE)

  ## Compute vertical width of the hexagon
  # Adjust for difference in width and height of regular hexagon. 1.15 adjusts
  # for the effect of the overlapping range in y-direction on the resolution
  dy <- ggplot2::resolution(hexdf_data$y, FALSE) / sqrt(3) / 2 * 1.15

  ## Obtain hexagon polygon coordinates
  hexC <- hexbin::hexcoords(dx, dy, n = 1) ## n: Number of hexagons repeat

  ## Obtain the number of hexagons in the full grid
  n <- length(hexdf_data$x)

  ## Generate the size vector of the hexagons (since regular hexagons)
  size <- rep(1, length(hexdf_data$x))

  ## Generate the coordinates for the hexagons
  full_hex_coords <- tibble::tibble( x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(hexdf_data$x, each = 6),
                                     y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(hexdf_data$y, each = 6),
                                     id = rep(1:length(hexdf_data$x), each = 6))

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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' map_hexbin_id(full_centroid_df, df_bin_centroids)
#'
#' @export
map_hexbin_id <- function(full_centroid_df, df_bin_centroids) {

  vec1 <- stats::setNames(rep("", 2), c("x", "y"))  ## Define column names

  ## Define a dataset to store all the centroids with the respective coordinates
  full_grid_with_hexbin_id <- dplyr::bind_rows(vec1)[0, ]
  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::mutate_if(is.character, as.numeric)

  for(i in 1:length(sort(unique(full_centroid_df$y)))){

    ## Filter the data set with specific y value
    specific_y_val_df <- full_centroid_df |>
      dplyr::filter(y == sort(unique(full_centroid_df$y))[i])

    ## orderd the x values
    ordered_x_df <- specific_y_val_df |>
      dplyr::arrange(x)

    full_grid_with_hexbin_id <- dplyr::bind_rows(full_grid_with_hexbin_id, ordered_x_df)

  }

  ## Add the column with hexagonal bin ID
  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::mutate(hexID = dplyr::row_number())

  ## Rename columns
  full_grid_with_hexbin_id <- full_grid_with_hexbin_id |>
    dplyr::rename("c_x" = "x",
                  "c_y" = "y")

  ## Join with centroid data set to extarct the count column
  full_grid_with_hexbin_id <- dplyr::full_join(full_grid_with_hexbin_id, df_bin_centroids, by = c("hexID" = "hexID")) |>
    dplyr::select(-c(x, y))

  ## Compute the standardise count
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' hex_grid <- full_hex_grid(full_centroid_df)
#' full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
#' map_polygon_id(full_grid_with_hexbin_id, hex_grid)
#'
#' @export
map_polygon_id <- function(full_grid_with_hexbin_id, hex_grid) {

  ## Define a dataset to store polygon id
  full_grid_with_polygon_id <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:length(unique(full_grid_with_hexbin_id$hexID))) {

    ## Filter specific hexagon
    full_grid_with_hexbin_id_filtered <- full_grid_with_hexbin_id |>
      dplyr::filter(hexID == unique(full_grid_with_hexbin_id$hexID)[i])

    for (j in 1:length(unique(hex_grid$id))) {

      ## Filter sepcific polygon
      hex_grid_filtered <- hex_grid |>
        dplyr::filter(id == unique(hex_grid$id)[j])

      ## Check the centroid exists within the polygon
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
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' generate_full_grid_info(df_bin_centroids)
#'
#' @export
generate_full_grid_info <- function(df_bin_centroids) {

  ## generate all the centroids
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  ## Map the hexgoanl ID to full centroid dataset
  full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)

  ## Generate all coordinates of hexagons
  hex_grid <- full_hex_grid(full_centroid_df)

  ## Map the polygon ID to the hexagon coordinates
  full_grid_with_polygon_id_df <- map_polygon_id(full_grid_with_hexbin_id, hex_grid)

  full_grid_with_hexbin_id_rep <- full_grid_with_polygon_id_df |>
    dplyr::slice(rep(1:dplyr::n(), each = 6)) |>
    dplyr::arrange(polygon_id)

  ## Generate the dataset with polygon, and hexagon bin centroid coordinates
  hex_full_count_df <- dplyr::bind_cols(hex_grid, full_grid_with_hexbin_id_rep)

  return(hex_full_count_df)

}


#' Find Points in Hexagonal Bins
#'
#' This function maps points to their corresponding hexagonal bins based on the provided data frames.
#'
#' @param full_grid_with_hexbin_id A data frame with hexagonal bin IDs and coordinates.
#' @param nldr_data_with_hb_id A data frame with 2D embedding data and hexagonal bin IDs.
#'
#' @return A data frame with hexagonal bin IDs and the corresponding points.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
#' hex_grid <- full_hex_grid(full_centroid_df)
#' full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
#' UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' find_pts_in_hexbins(full_grid_with_hexbin_id, nldr_data_with_hb_id = UMAP_data_with_hb_id)
#'
#' @export
find_pts_in_hexbins <- function(full_grid_with_hexbin_id, nldr_data_with_hb_id) {

  ## Dataframe to store points info
  pts_df <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:length(nldr_data_with_hb_id$hb_id)) {

    ## Filter a hexagon and find the point within that hexagon
    pts_vec <- nldr_data_with_hb_id |>
      dplyr::filter(hb_id == nldr_data_with_hb_id$hb_id[i]) |>
      dplyr::pull(ID)

    ## Store the hexagon ID with the respective points
    hb_pts <- tibble::tibble(hexID = nldr_data_with_hb_id$hb_id[i], pts = list(pts_vec))

    pts_df <- dplyr::bind_rows(pts_df, hb_pts)

  }

  return(pts_df)

}

#' Find the Number of Non-Empty Bins Required
#'
#' This function determines the number of non-empty bins needed to satisfy a minimum requirement.

#' @param nldr_df A data frame containing 2D embeddings.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param non_empty_bins The desired number of non-empty bins.
#'
#' @return The number of bins along the x-axis needed to achieve the specified number of non-empty bins.
#'
#' @examples
#' shape_value <- 1.833091
#' non_empty_bins <- 7
#' find_non_empty_bins(nldr_df = s_curve_noise_umap, x = "UMAP1", y = "UMAP2",
#' shape_val = shape_value, non_empty_bins)
#'
#' @export
find_non_empty_bins <- function(nldr_df, x = "UMAP1", y = "UMAP2", shape_val, non_empty_bins) {

  num_bins_x <- 1
  ## To extract bin centroids
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = nldr_df,
                                                 num_bins = num_bins_x,
                                                 shape_val = shape_val, x = x, y = y)
  df_bin_centroids <- hexbin_data_object$hexdf_data

  num_of_non_empty_bins <- df_bin_centroids$hexID |> length()

  while (num_of_non_empty_bins < non_empty_bins) {

    num_bins_x <- num_bins_x + 1

    ## To extract bin centroids
    hexbin_data_object <- extract_hexbin_centroids(nldr_df = nldr_df,
                                                   num_bins = num_bins_x,
                                                   shape_val = shape_val, x = y, y = y)

    df_bin_centroids <- hexbin_data_object$hexdf_data

    num_of_non_empty_bins <- df_bin_centroids$hexID |> length()

    if (num_of_non_empty_bins >= non_empty_bins) {
      return(num_bins_x)
      break
    } else {
      next
    }
  }
}

#' Extract Coordinates of Shifted Hexagonal Grid
#'
#' This function takes input data, which includes a hexbin ID, and extracts the coordinates
#' of a hexagonal grid with a specified shift. The resulting dataset includes hexagon centroids
#' with updated coordinates and additional information such as counts within each hexagon.
#'
#' @param nldr_data_with_hb_id A containing 2D embeddings with a hexbin ID.
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#' @param hex_full_count_df A data frame with information about all hexagonal grid cells.
#' @param shift The value that centroids need to be shifted. If not provided, it is calculated
#'   as half of the cell diameter of a hexagon.
#'
#' @return A data frame with updated hexagon coordinates, hexagon IDs, and counts within each hexagon.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' hex_full_count_df <- generate_full_grid_info(df_bin_centroids)
#' UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' extract_coord_of_shifted_hex_grid(nldr_data_with_hb_id = UMAP_data_with_hb_id,
#' num_bins_x = num_bins_x, hex_full_count_df)
#'
#' @export
extract_coord_of_shifted_hex_grid <- function(nldr_data_with_hb_id, num_bins_x, hex_full_count_df, shift = NA) {

  if (is.na(shift)) {
    cell_diameter <- sqrt(2 * 1 / sqrt(3))
    shift <- cell_diameter/2

  }

  ## Filter centroids with their hexIDs
  hexbin_coord_all <- hex_full_count_df |>
    dplyr::select(c_x, c_y, hexID) |>
    dplyr::distinct()

  hexbin_coord_all_new <- hexbin_coord_all |>
    dplyr::mutate(c_x = c_x - shift,
                  c_y = c_y - shift) |>
    dplyr::rename(c("x" = "c_x",
                    "y" = "c_y"))

  ## Generate all coordinates of hexagons
  hex_grid_new <- full_hex_grid(hexbin_coord_all_new)

  hexbin_coord_all_new <- hexbin_coord_all_new |>
    dplyr::rename(c("c_x" = "x",
                    "c_y" = "y"))

  ## Map the polygon ID to the hexagon coordinates
  full_grid_with_polygon_id_df <- map_polygon_id(hexbin_coord_all_new, hex_grid_new)

  full_grid_with_hexbin_id_rep <- full_grid_with_polygon_id_df |>
    dplyr::slice(rep(1:dplyr::n(), each = 6)) |>
    dplyr::arrange(polygon_id)

  ## Generate the dataset with polygon, and hexagon bin centroid coordinates
  hex_full_count_df_new <- dplyr::bind_cols(hex_grid_new, full_grid_with_hexbin_id_rep)

  ## Datafarme to store new hexIDs
  nldr_df_with_new_hexID <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:NROW(nldr_data_with_hb_id)) {

    ## Select the nldr point
    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id |>
      dplyr::filter(dplyr::row_number() == i)

    ## Find nearest hexIDs
    df_bin_centroids_coordinates_spec_bin_near1 <- hexbin_coord_all_new |>
      dplyr::filter((hexID == nldr_data_with_hb_id_spec$hb_id[1]) |(hexID == (nldr_data_with_hb_id_spec$hb_id[1] + (num_bins_x + 1))) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] + num_bins_x)) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] - (num_bins_x + 1))) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] - num_bins_x)))

    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id_spec |>
      dplyr::select(-ID) |>
      dplyr::rename("x" = names(nldr_data_with_hb_id_spec)[1],
                    "y" = names(nldr_data_with_hb_id_spec)[2])

    df_bin_centroids_coordinates_spec_bin_near1 <- df_bin_centroids_coordinates_spec_bin_near1 |>
      dplyr::rename("x" = "c_x",
                    "y" = "c_y",
                    "hb_id" = "hexID")

    near_df_1 <- dplyr::bind_rows(nldr_data_with_hb_id_spec, df_bin_centroids_coordinates_spec_bin_near1)

    ## Compute the distance from selected point to neighbouring centroids
    near_df_1$distance <- lapply(seq(nrow(near_df_1)), function(x) {
      start <- unlist(near_df_1[1, c("x","y")])
      end <- unlist(near_df_1[x, c("x","y")])
      sqrt(sum((start - end)^2))})

    near_df_1$distance <- unlist(near_df_1$distance)

    near_df_1 <- near_df_1 |>
      dplyr::filter(dplyr::row_number() != 1) |>
      dplyr::arrange(distance)

    ## Select the most nearest centroid and assign the hexID of that centroid
    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id_spec |>
      dplyr::select(-hb_id) |>
      dplyr::mutate(hb_id = near_df_1$hb_id[1])

    nldr_df_with_new_hexID <- dplyr::bind_rows(nldr_df_with_new_hexID, nldr_data_with_hb_id_spec)

  }


  ## Find counts within each hexagon
  hb_id_with_counts <- nldr_df_with_new_hexID |>
    dplyr::count(hb_id) |>
    dplyr::mutate(counts = n,
                  std_counts = n/max(n)) |>
    dplyr::select(-n)

  hex_full_count_df_new <- dplyr::left_join(hex_full_count_df_new, hb_id_with_counts,
                                            by = c("hexID" = "hb_id"))

  return(hex_full_count_df_new)

}


