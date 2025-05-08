#' Generate centroid coordinate
#'
#' This function generates all possible centroids in the hexagonal grid.
#'
#' @param nldr_data A tibble that contains embedding components in the first and second columns.
#' @param bin1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A tibble contains hexIDs, x and y coordinates (hexID, c_x, c_y respectively)
#' of all hexagon bin centroids.
#'
#' @examples
#' gen_centroids(nldr_obj = scurve_umap_obj, bin1 = 4, q = 0.1)
#'
#' @export
gen_centroids <- function(nldr_obj, bin1 = 4, q = 0.1){

  ## To check whether bin2 greater than 2
  if (bin1 < 2) {
    cli::cli_abort("Number of bins along the x-axis at least should be 2.")
  }

  ## To check whether q is between a specific range
  if (!dplyr::between(q, 0.05, 0.2)) {
    cli::cli_abort("The buffer should be within 0.05 and 0.2.")
  }

  ## To compute the range
  lim1 <- nldr_obj$lim1
  lim2 <- nldr_obj$lim2
  r2 <- diff(lim2)/diff(lim1)

  ## To compute hexagonal configurations
  bin_obj <- calc_bins_y(nldr_obj = nldr_obj, bin1 = bin1, q = q)

  # To obtain the bins along the y-axis
  bin2 <- bin_obj$bin2

  # To obtain the width of the hexagon
  a1 <- bin_obj$a1

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  # Generate x-coordinate of centroids for odd rows
  c_x_vec_odd <- seq(s1, by = a1, length.out = bin1) ## since x range is 0-1

  # Generate x-coordinate of centroids for even rows
  c_x_vec_even <- c_x_vec_odd + a1/2
  c_x_vec <- c(c_x_vec_odd, c_x_vec_even)

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  # Generate y-coordinate of centroids
  c_y_vec <- seq(s2, by = a2, length.out = bin2)
  c_y <- rep(c_y_vec, each = bin1)

  ## Do the number of belongs y axis is even or odd and adjust the x-coordinates
  if ((bin2 %% 2) == 0) {

    c_x <- rep(c_x_vec, bin2/2)

  } else {

    c_x <- append(rep(c_x_vec, floor(bin2/2)), c_x_vec_odd)

  }

  centroids_data <- tibble::tibble(hexID = 1:length(c_x), c_x = c_x, c_y = c_y)

  return(centroids_data)

}

#' Generate hexagonal polygon coordinates
#'
#' This function generates the coordinates of hexagons after passing
#' hexagonal centroids.
#'
#' @param centroids_data The dataset with all hexagonal bin IDs
#' and centroid coordinates.
#' @param a1 The width of the hexagon.
#'
#' @return A tibble contains polygon id, x and y coordinates (hex_poly_id, x,
#' and y respectively) of hexagons.
#'
#' @examples
#' width <- scurve_model_obj$hb_obj$a1
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' gen_hex_coord(centroids_data = all_centroids_df, a1 = width)
#'
#' @export
gen_hex_coord <- function(centroids_data, a1){

  # If the hexagonal width is missing
  if (missing(a1)) {
    cli::cli_abort("Need to initialize the width of the hexagon.")
  }

  ## Obtain centroid info
  hex_ids <- centroids_data$hexID
  c_x_vec <- centroids_data$c_x
  c_y_vec <- centroids_data$c_y

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

  for (hexID in hex_ids) {

    ## Since each hexagon has 6 coordinates
    hexID_rep <- rep(hex_ids[hexID], each = 6)
    c_x_rep <- rep(c_x_vec[hexID], each = 6)
    c_y_rep <- rep(c_y_vec[hexID], each = 6)

    ## Generate the 6 coordinates
    x_spec <- c_x_rep + x_add_factor
    y_spec <- c_y_rep + y_add_factor

    ## Append to existing vectors
    x <- append(x, x_spec)
    y <- append(y, y_spec)
    hex_poly_id <- append(hex_poly_id, hexID_rep)

  }

  hex_coord_df <- tibble::tibble(hex_poly_id = hex_poly_id, x = x, y = y)

  return(hex_coord_df)
}

#' Get indices of all minimum distances
#'
#' This function returns the indices of all minimum distances.
#'
#' @param x A numeric vector.
#' @return A numeric vector containing the indices of all minimum distances.
#' @examples
#' x <- c(1, 2, 1, 3)
#' get_min_indices(x)
#' @export
get_min_indices <- function(x) {
  min_indices <- which(abs(x - min(x)) < 0.00001, arr.ind = TRUE)
  # If there are multiple minimum values, return the minimum index
  if (length(min_indices) > 1) {
    min_indices <- min(min_indices)
  }
  return(min_indices)
}


#' Assign data to hexagons
#'
#' This function assigns the data to hexagons.
#'
#' @param nldr_data A tibble that contains embedding components in the first and second columns.
#' @param centroids_data The dataset with centroid coordinates.
#'
#' @return A tibble contains embedding components and corresponding hexagon ID.
#'
#' @examples
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' assign_data(nldr_obj = scurve_umap_obj, centroids_data = all_centroids_df)
#'
#' @export
assign_data <- function(nldr_obj, centroids_data) {

  scaled_nldr_df <- nldr_obj$scaled_nldr

  ## To select first and second embedding components
  select_emb <- scaled_nldr_df |>
    dplyr::select(c(1, 2))

  ## To select coordinates for the centroids
  select_centroid <- centroids_data |>
    dplyr::select(c(2, 3))

  ## Convert to a matrix
  matrix_nldr <- as.matrix(select_emb)
  centroid_matrix <- as.matrix(select_centroid)

  ## Compute distances between embedding points and hex bin centroids
  dist_df <- proxy::dist(matrix_nldr, centroid_matrix, method = "Euclidean")

  # Get the column indices of minimum distances (if there are multiple minimum,
  # get the minimum indicies)
  min_column <- apply(dist_df, 1, get_min_indices)

  # Extract hex bin IDs corresponding to minimum distances
  hb_ids <- centroids_data$hexID[min_column]

  # Add hex bin IDs to the data
  scaled_nldr_df <- scaled_nldr_df |>
    dplyr::mutate(hexID = hb_ids)

  return(scaled_nldr_df)
}

#' Compute standardize counts in hexagons
#'
#' This function computes the standardize number of points within each hexagon.
#'
#' @param scaled_nldr_hexid A tibble that contains the scaled embedding with hexagonal bin IDs.
#'
#' @return A tibble that contains hexagon IDs and the corresponding standardize counts.
#' @importFrom dplyr count mutate rename
#'
#' @examples
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' compute_std_counts(scaled_nldr_hexid = umap_with_hb_id)
#'
#' @export
compute_std_counts <- function(scaled_nldr_hexid) {

  ## Group by hexagon IDs
  std_df <- scaled_nldr_hexid |>
    count(hexID) |>
    mutate(std_counts = n/max(n)) |>
    rename(bin_counts = n)

  return(std_df)

}

#' Find points in hexagonal bins
#'
#' This function maps points to their corresponding hexagonal bins.
#'
#' @param scaled_nldr_hexid A tibble that contains the scaled embedding with hexagonal bin IDs.
#'
#' @return A tibble with hexagonal bin IDs and the corresponding points.
#' @importFrom dplyr filter pull summarize
#' @importFrom tibble tibble
#'
#' @examples
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' find_pts(scaled_nldr_hexid = umap_with_hb_id)
#'
#' @export
find_pts <- function(scaled_nldr_hexid) {

  pts_df <- scaled_nldr_hexid |>
    group_by(hexID) |>
    summarize(pts_list = list(ID), .groups = "drop")

  return(pts_df)

}


#' Hexagonal binning
#'
#' This function generates the hexagonal object.
#'
#' @param nldr_data A tibble that contains embedding with a unique identifier.
#' @param bin1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A object that contains numeric vector that contains binwidths (a1),
#' vertical distance (a2), bins along the x and y axes respectively (bins),
#' numeric vector that contains hexagonal
#' starting point coordinates all hexagonal bin centroids (centroids),
#' hexagonal coordinates of the full grid(hex_poly),
#' embedding components with their corresponding hexagon IDs (data_hb_id),
#' hex bins with their corresponding standardise counts (std_cts),
#' total number of hex bins(tot_bins), number of non-empty hex bins (non_bins)
#' and points within each hexagon (pts_bins).
#'
#'
#' @examples
#' hex_binning(nldr_obj = scurve_umap_obj, bin1 = 4, q = 0.1)
#'
#' @export
hex_binning <- function(nldr_obj, bin1 = 4, q = 0.1) {

  scaled_nldr <- nldr_obj$scaled_nldr

  ## To compute the range
  lim1 <- nldr_obj$lim1
  lim2 <- nldr_obj$lim2
  r2 <- diff(lim2)/diff(lim1)

  ## To compute the number of bins along the y-axis
  bin_obj <- calc_bins_y(nldr_obj = nldr_obj, bin1 = bin1, q = q)
  bin2 <- bin_obj$bin2

  ## To obtain the width of the hexagon
  a1 <- bin_obj$a1

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  ## To generate all the centroids of the grid
  all_centroids_df <- gen_centroids(nldr_obj = nldr_obj, bin1 = bin1, q = q)

  ## To generate the hexagon coordinates
  all_hex_coord <- gen_hex_coord(centroids_data = all_centroids_df, a1 = a1)

  ## To find which 2D embedding assigned to which hexagon
  nldr_hex_id <- assign_data(nldr_obj = nldr_obj, centroids_data = all_centroids_df)

  ## To generate standardize counts of each hexagon
  std_df <- compute_std_counts(scaled_nldr_hexid = nldr_hex_id)

  ## To find which points are within each hexagon
  pts_df <- find_pts(scaled_nldr_hexid = nldr_hex_id)

  ## To generate the object of hexagon info
  hex_bin_obj <- list(a1 = a1,
                      a2 = a2,
                      bins = c(bin1, bin2),
                      start_point = c(s1, s2),
                      centroids = all_centroids_df,
                      hex_poly = all_hex_coord,
                      data_hb_id = nldr_hex_id,
                      std_cts = std_df,
                      tot_bins = NROW(all_centroids_df),
                      non_bins = length(std_df$hexID),
                      pts_bins = pts_df
  )
  class(hex_bin_obj) <- "hex_bin_obj"

  return(hex_bin_obj)

}


#' Extract hexagonal bin centroids coordinates and the corresponding standardise counts.
#'
#' @param centroids_data A tibble that contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#' @param counts_data A tibble that contains hexagon IDs with the standardise
#' number of points within each hexagon.
#'
#' @return A tibble contains hexagon ID, centroid coordinates, and standardise counts.
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_data <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' extract_hexbin_centroids(centroids_data = all_centroids_df,
#' counts_data = counts_data)
#'
#' @export
extract_hexbin_centroids <- function(centroids_data, counts_data) {

  ## To arrange the hexagon IDs
  counts_data <- counts_data |>
    dplyr::arrange(hexID)

  ## To join the datasets
  centroids_data <- dplyr::full_join(centroids_data, counts_data, by = "hexID")

  ## Map the standardize counts
  centroids_data <- centroids_data |>
    dplyr::mutate(std_counts = dplyr::if_else(is.na(std_counts), 0, std_counts)) |>
    dplyr::mutate(bin_counts = dplyr::if_else(is.na(bin_counts), 0, bin_counts))

  return(centroids_data)
}

#' Extract hexagonal bin mean coordinates and the corresponding standardize counts.
#'
#' @param data_hb A tibble with embedding components and hexagonal bin IDs.
#' @param counts_data A tibble that contains hexagon IDs with the standardise
#' number of points within each hexagon.
#' @param centroids_data A tibble that contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#'
#' @return A tibble contains hexagon ID, bin mean coordinates, and standardize counts.
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_data <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' extract_hexbin_mean(data_hb = umap_with_hb_id, counts_data = counts_data,
#' centroids_data = all_centroids_df)
#'
#' @export
extract_hexbin_mean <- function(data_hb, counts_data, centroids_data) {

  ## To arrange the hexagon IDs
  counts_data <- counts_data |>
    dplyr::arrange(hexID)

  ## To join the datasets
  centroids_data <- dplyr::full_join(centroids_data, counts_data, by = "hexID") |>
    dplyr::select(-c(c_x, c_y))

  ## To compute hexagonal bin means
  hex_mean_df <- data_hb |>
    dplyr::select(-ID) |>
    dplyr::group_by(hexID) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean)) |>
    dplyr::arrange(hexID)

  ## Rename columns
  names(hex_mean_df) <- c("hexID", "c_x", "c_y")

  centroids_data <- dplyr::full_join(centroids_data, hex_mean_df, by = c("hexID" = "hexID")) |>
    dplyr::rename(bin_counts = n) |>
    dplyr::select(hexID, c_x, c_y, bin_counts, std_counts)

  return(centroids_data)
}

#' Triangulate bin centroids
#'
#' This function triangulates the bin centroids using the x and y coordinates
#' provided in the input data frame and returns the triangular object.
#'
#' @param centroids_data The tibble containing the all the bin centroids.
#'
#' @return A triangular object representing the triangulated bin centroids.
#' @importFrom interp tri.mesh
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_data <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' df_bin_centroids <- extract_hexbin_mean(data_hb = umap_with_hb_id, counts_data = counts_data,
#' centroids_data = all_centroids_df)
#' tri_bin_centroids(centroids_data = df_bin_centroids)
#'
#' @export
tri_bin_centroids <- function(centroids_data){
  tr <- tri.mesh(centroids_data[["c_x"]], centroids_data[["c_y"]])

  # Create a list to store the triangulation object and the counts
  result <- list(
    trimesh_object = tr,
    bin_counts = centroids_data[["bin_counts"]]
  )

  return(result)
}

#' Calculate 2D Euclidean distances between vertices
#'
#' This function calculates the 2D distances between pairs of points in a data frame.
#'
#' @param tr_coord_df A tibble that contains the x and y coordinates of start
#' and end points.
#' @param start_x Column name for the x-coordinate of the starting point.
#' @param start_y Column name for the y-coordinate of the starting point.
#' @param end_x Column name for the x-coordinate of the ending point.
#' @param end_y Column name for the y-coordinate of the ending point.
#' @param select_vars A character vector specifying the columns to be
#' selected in the resulting data frame.
#'
#' @return A tibble with columns for the starting point, ending point,
#' and calculated distances.
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#'
#' @examples
#' tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
#' cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from", start_y = "y_from",
#' end_x = "x_to", end_y = "y_to", select_vars = c("from", "to", "distance"))
#'
#' @export
calc_2d_dist <- function(tr_coord_df, start_x, start_y, end_x, end_y,
                        select_vars) {

  # Calculate the 2D distances
  tr_coord_df$distance <- lapply(seq(nrow(tr_coord_df)), function(x) {
    start <- unlist(tr_coord_df[x, c(start_x, start_y)], use.names = FALSE)
    end <- unlist(tr_coord_df[x, c(end_x, end_y)], use.names = FALSE)
    sqrt(sum((start - end)^2))
  })

  # Create a data frame with the from-to relationships and distances
  tr_coord_df <- tr_coord_df |>
    select(from, to, distance)

  # Convert the distances to a vector and return the data frame
  tr_coord_df$distance <- unlist(tr_coord_df$distance, use.names = FALSE)
  return(tr_coord_df)
}

#' Generate edge information
#'
#' This function generates edge information from a given triangular object,
#' including the coordinates of the vertices and the from-to relationships
#' between the vertices.
#'
#' @param tri_object The triangular object from which to generate edge information.
#'
#' @return A tibble that contains the edge information, including the from-to
#' relationships and the corresponding x and y coordinates.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter rename distinct left_join select
#' @importFrom interp triangles
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_data <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' df_bin_centroids <- extract_hexbin_mean(data_hb = umap_with_hb_id, counts_data = counts_data,
#' centroids_data = all_centroids_df)
#' tr1_object <- tri_bin_centroids(centroids_data = df_bin_centroids)
#' gen_edges(tri_object = tr1_object)
#'
#' @export
gen_edges <- function(tri_object) { #centroids_data
  # Access the trimesh object and bin counts
  tri <- tri_object$trimesh_object
  counts <- tri_object$bin_counts

  # Create a data frame with x, y coordinates, and bin counts
  tr_df <- tibble(
    x = tri$x,
    y = tri$y,
    ID = 1:length(tri$x),
    n_obs = counts # Assuming 'n_obs' is how you named the counts
  )

  # Extract the triangles
  trang <- triangles(tri)
  trang <- as_tibble(trang)

  # Create initial from-to edges
  tr_arcs_df <- tibble(
    from = c(trang$node1, trang$node1, trang$node2),
    to = c(trang$node2, trang$node3, trang$node3)
  )

  # Filter edges based on the bin counts of the connected nodes
  edges_all <- tr_arcs_df |>
    left_join(tr_df |> select(ID, n_obs), by = c("from" = "ID")) |>
    rename(from_count = n_obs) |>
    left_join(tr_df |> select(ID, n_obs), by = c("to" = "ID")) |>
    rename(to_count = n_obs) |>
    select(from, to) |>
    mutate(x = pmin(from, to), y = pmax(from, to)) |>
    distinct(x, y) |>
    rename(from = x, to = y)

  # Map from and to coordinates for the filtered edges
  tr_from_to_df_coord <- left_join(edges_all, tr_df, by = c("from" = "ID")) |>
    rename(x_from = x, y_from = y, from_count = n_obs) |>
    left_join(tr_df, by = c("to" = "ID")) |>
    rename(x_to = x, y_to = y, to_count = n_obs) |>
    select(from, to, x_from, y_from, x_to, y_to, from_count, to_count) # Keep only necessary columns

  ## Updated the from and to
  # Find the unique values in `from` and `to`, and sort them.
  unique_values <- sort(unique(c(tr_from_to_df_coord$from, tr_from_to_df_coord$to)))

  # Create a mapping between the old values and the new, renumbered values (starting from 1).
  value_map <- tibble::tibble(old_value = unique_values, new_value = 1:length(unique_values))

  # Join this mapping to your data frame to replace the old values with the new ones.
  tr_from_to_df_coord <- tr_from_to_df_coord |>
    left_join(value_map, by = c("from" = "old_value")) |>
    mutate(from = new_value) |>
    select(-new_value) |> # Remove the temporary column
    left_join(value_map, by = c("to" = "old_value")) |>
    mutate(to = new_value) |>
    select(-new_value)    # Remove the temporary column

  return(tr_from_to_df_coord)
}

#' Visualize triangular mesh after removing the long edges
#'
#' This function visualize the triangular mesh after removing the long edges.
#'
#' @param trimesh_data A tibble that contains the x and y coordinates of start and end points.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are removed.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes labs
#' @importFrom tibble tibble
#'
#' @examples
#' tr_from_to_df <- scurve_model_obj$trimesh_data
#' vis_mesh(trimesh_data = tr_from_to_df)
#'
#' @export
vis_mesh <- function(trimesh_data) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(trimesh_data[["x_from"]], trimesh_data[["x_to"]]),
                          y = c(trimesh_data[["y_from"]], trimesh_data[["y_to"]])) |>
    distinct()

  ## Create the triangular mesh plot after removing the long edges
  tri_mesh_plot <- ggplot(tr_df, aes(x = x, y = y)) +
    geom_segment(aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                          data = trimesh_data,
                 colour = "#33a02c") +
    geom_point(size = 1, colour = "#33a02c") +
    labs(color=NULL)
  return(tri_mesh_plot)

}

#' Find the number of bins required to achieve required number of non-empty bins.
#'
#' This function determines the number of bins along the x and y axes
#' to obtain a specific number of non-empty bins.
#'
#' @param data A tibble that contains the embedding.
#' @param non_empty_bins The desired number of non-empty bins.
#' @param q The buffer amount as proportion of data range.
#'
#' @return The number of bins along the x and y axes
#' needed to achieve a specific number of non-empty bins.
#'
#' @examples
#' find_non_empty_bins(nldr_obj = scurve_umap_obj, non_empty_bins = 5)
#'
#' @export
find_non_empty_bins <- function(nldr_obj, non_empty_bins = 2, q = 0.1) {

  ## To check whether q is between a specific range
  if (!dplyr::between(q, 0.05, 0.2)) {
    cli::cli_abort("The buffer should be within 0.05 and 0.2.")
  }

  scaled_nldr_data <- nldr_obj$scaled_nldr

  max_bins_along_axis <- ceiling(sqrt(NROW(scaled_nldr_data)))

  ## Since having 1 bin along x or y-axis is not obvious therefore started from 2
  num_bins_x_vec <- 4:max_bins_along_axis

  ## To initialise the number of bins along the x-axis
  bin1 <- num_bins_x_vec[1]

  ### Generate the full grid
  hb_obj <- hex_binning(nldr_obj = nldr_obj, bin1 = bin1, q = q)

  ## To compute the number of bins along the y-axis
  bin2 <- hb_obj$bins[2]

  num_of_non_empty_bins <- hb_obj$non_bins

  i <- 1

  while (num_of_non_empty_bins < non_empty_bins) {

    i <- i + 1

    if (length(num_bins_x_vec) < i) {
      stop("There is no matching number of bins along the x and y axes
           for the required number of non empty bins.")
    }

    bin1 <- num_bins_x_vec[2]

    ### Generate the full grid
    hb_obj <- hex_binning(nldr_obj = nldr_obj, bin1 = bin1, q = q)

    ## To compute the number of bins along the y-axis
    bin2 <- hb_obj$bins[2]

    num_of_non_empty_bins <- hb_obj$non_bins

    if (num_of_non_empty_bins >= non_empty_bins) {
      return(list(bin1 = bin1, bin2 = bin2))
      break
    } else {
      next
    }
  }
}

