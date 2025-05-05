#' Generate centroid coordinate
#'
#' This function generates all possible centroids in the hexagonal grid.
#'
#' @param bin1 Number of bins along the x axis.
#' @param r2 The ratio of the ranges of the original embedding components.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A tibble contains hexIDs, x and y coordinates (hexID, c_x, c_y respectively)
#' of all hexagon bin centroids.
#' @importFrom tibble tibble
#'
#' @examples
#' scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
#' lim1 <- scurve_umap_scaled_obj$lim1
#' lim2 <- scurve_umap_scaled_obj$lim2
#' r2 <- diff(lim2)/diff(lim1)
#' gen_centroids(bin1 = 4, r2 = r2, q = 0.1)
#'
#' @export
gen_centroids <- function(bin1 = 2, r2, q = 0.1){

  ## To compute hexagonal configurations
  bin_obj <- calc_bins_y(bin1 = bin1, r2 = r2, q = q)

  # To obtain the bins along the y-axis
  bin2 <- bin_obj$bin2

  # To obtain the width of the hexagon
  a1 <- bin_obj$a1

  ## To check whether q is between a specific range
  if (!between(q, 0.05, 0.2)) {
    stop("The buffer should be within 0.05 and 0.2.")
  }

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

  centroid_df <- tibble(hexID = 1:length(c_x), c_x = c_x, c_y = c_y)

  return(centroid_df)

}

#' Generate hexagonal polygon coordinates
#'
#' This function generates the coordinates of hexagons after passing
#' hexagonal centroids.
#'
#' @param centroids_df The dataset with all hexagonal bin IDs
#' and centroid coordinates.
#' @param a1 The width of the hexagon.
#'
#' @return A tibble contains polygon id, x and y coordinates (hex_poly_id, x,
#' and y respectively) of hexagons.
#' @importFrom tibble tibble
#'
#' @examples
#' width <- s_curve_obj$s_curve_umap_hb_obj$a1
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' gen_hex_coord(centroids_df = all_centroids_df, a1 = width)
#'
#' @export
gen_hex_coord <- function(centroids_df, a1){

  # If the hexagonal width is missing
  if (missing(a1)) {
    stop("Need to initialize the width of the hexagon.")
  }

  ## Obtain centroid info
  hex_ids <- centroids_df$hexID
  c_x_vec <- centroids_df$c_x
  c_y_vec <- centroids_df$c_y

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

  hex_coord_df <- tibble(hex_poly_id = hex_poly_id, x = x, y = y)

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
#' @param data data A tibble or data frame.
#' @param centroid_df The dataset with centroid coordinates.
#'
#' @return A tibble contains embedding components and corresponding hexagon ID.
#' @importFrom proxy dist
#' @importFrom dplyr mutate select
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' assign_data(data = s_curve_noise_umap_scaled, centroid_df = all_centroids_df)
#'
#' @export
assign_data <- function(data, centroid_df) {

  ## To select first and second embedding components
  select_emb <- data |>
    select(c(1, 2))

  ## To select coordinates for the centroids
  select_centroid <- centroid_df |>
    select(c(2, 3))

  ## Convert to a matrix
  matrix_nldr <- as.matrix(select_emb)
  centroid_matrix <- as.matrix(select_centroid)

  ## Compute distances between embedding points and hex bin centroids
  dist_df <- proxy::dist(matrix_nldr, centroid_matrix, method = "Euclidean")

  # Get the column indices of minimum distances (if there are multiple minimum,
  # get the minimum indicies)
  min_column <- apply(dist_df, 1, get_min_indices)

  # Extract hex bin IDs corresponding to minimum distances
  hb_ids <- centroid_df$hexID[min_column]

  # Add hex bin IDs to the data
  data <- data |>
    mutate(hb_id = hb_ids)

  return(data)
}

#' Compute standardize counts in hexagons
#'
#' This function computes the standardize number of points within each hexagon.
#'
#' @param data_hb A tibble with embedding and hexagonal bin IDs.
#'
#' @return A tibble that contains hexagon IDs and the corresponding standardize counts.
#' @importFrom dplyr count mutate
#'
#' @examples
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' compute_std_counts(data_hb = umap_with_hb_id)
#'
#' @export
compute_std_counts <- function(data_hb) {

  ## Group by hexagon IDs
  std_df <- data_hb |>
    count(hb_id) |>
    mutate(std_counts = n/max(n))

  return(std_df)

}

#' Find points in hexagonal bins
#'
#' This function maps points to their corresponding hexagonal bins.
#'
#' @param data_hb A data frame with data, ID and hexagonal bin IDs.
#'
#' @return A tibble with hexagonal bin IDs and the corresponding points.
#' @importFrom dplyr filter pull summarize
#' @importFrom tibble tibble
#'
#' @examples
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' find_pts(data_hb = umap_with_hb_id)
#'
#' @export
find_pts <- function(data_hb) {

  pts_df <- data_hb |>
    group_by(hb_id) |>
    summarize(pts_list = list(ID), .groups = "drop")

  return(pts_df)

}


#' Hexagonal binning
#'
#' This function generates the hexagonal object.
#'
#' @param data A tibble that contains embedding components.
#' @param bin1 Number of bins along the x axis.
#' @param r2 The ratio of the ranges of the original embedding components.
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
#' scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
#' lim1 <- scurve_umap_scaled_obj$lim1
#' lim2 <- scurve_umap_scaled_obj$lim2
#' r2 <- diff(lim2)/diff(lim1)
#' num_bins_x <- 4
#' hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2, q = 0.1)
#'
#' @export
hex_binning <- function(data, bin1 = 4, r2, q = 0.1) {

  ## To compute the number of bins along the y-axis
  bin_obj <- calc_bins_y(bin1 = bin1, r2 = r2, q = q)
  bin2 <- bin_obj$bin2

  ## To obtain the width of the hexagon
  a1 <- bin_obj$a1

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  ## To generate all the centroids of the grid
  all_centroids_df <- gen_centroids(bin1 = bin1, r2 = r2, q = q)

  ## To generate the hexagon coordinates
  all_hex_coord <- gen_hex_coord(centroids_df = all_centroids_df, a1 = a1)

  ## To find which 2D embedding assigned to which hexagon
  nldr_hex_id <- assign_data(data = data, centroid_df = all_centroids_df)

  ## To generate standardize counts of each hexagon
  std_df <- compute_std_counts(data_hb = nldr_hex_id)

  ## To find which points are within each hexagon
  pts_df <- find_pts(data_hb = nldr_hex_id)

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
                      non_bins = length(std_df$hb_id),
                      pts_bins = pts_df
  )
  class(hex_bin_obj) <- "hex_bin_obj"

  return(hex_bin_obj)

}


#' Extract hexagonal bin centroids coordinates and the corresponding standardise counts.
#'
#' @param centroids_df A tibble that contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#' @param counts_df A tibble that contains hexagon IDs with the standardise
#' number of points within each hexagon.
#'
#' @return A tibble contains hexagon ID, centroid coordinates, and standardise counts.
#' @importFrom dplyr arrange mutate if_else full_join
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_df <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df)
#'
#' @export
extract_hexbin_centroids <- function(centroids_df, counts_df) {

  ## To arrange the hexagon IDs
  counts_df <- counts_df |>
    arrange(hb_id)

  ## To join the datasets
  centroids_df <- full_join(centroids_df, counts_df, by = c("hexID" = "hb_id"))

  ## Map the standardize counts
  centroids_df <- centroids_df |>
    mutate(drop_empty = if_else(!(is.na(std_counts)), FALSE, TRUE)) |>
    rename(bin_counts = n)

  return(centroids_df)
}

#' Extract hexagonal bin mean coordinates and the corresponding standardize counts.
#'
#' @param data_hb A tibble with embedding components and hexagonal bin IDs.
#' @param counts_df A tibble that contains hexagon IDs with the standardise
#' number of points within each hexagon.
#' @param centroids_df A tibble that contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#'
#' @return A tibble contains hexagon ID, bin mean coordinates, and standardize counts.
#' @importFrom dplyr select arrange group_by summarise filter mutate across full_join
#' @importFrom tidyselect everything
#'
#' @examples
#' all_centroids_df <- s_curve_obj$s_curve_umap_hb_obj$centroids
#' counts_df <- s_curve_obj$s_curve_umap_hb_obj$std_cts
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' extract_hexbin_mean(data_hb = umap_with_hb_id, counts_df = counts_df,
#' centroids_df = all_centroids_df)
#'
#' @export
extract_hexbin_mean <- function(data_hb, counts_df, centroids_df) {

  ## To arrange the hexagon IDs
  counts_df <- counts_df |>
    arrange(hb_id)

  ## To join the datasets
  centroids_df <- full_join(centroids_df, counts_df, by = c("hexID" = "hb_id")) |>
    select(-c(c_x, c_y))

  ## To compute hexagonal bin means
  hex_mean_df <- data_hb |>
    select(-ID) |>
    group_by(hb_id) |>
    summarise(across(everything(), mean)) |>
    arrange(hb_id)

  ## Rename columns
  names(hex_mean_df) <- c("hexID", "c_x", "c_y")

  centroids_df <- full_join(centroids_df, hex_mean_df, by = c("hexID" = "hexID")) |>
    mutate(drop_empty = if_else(!(is.na(std_counts)), FALSE, TRUE)) |>
    rename(bin_counts = n) |>
    select(hexID, c_x, c_y, bin_counts, std_counts, drop_empty)

  return(centroids_df)
}

#' Triangulate bin centroids
#'
#' This function triangulates the bin centroids using the x and y coordinates
#' provided in the input data frame and returns the triangular object.
#'
#' @param hex_df The tibble containing the bin centroids.
#' @param x The name of the column that contains x coordinates of bin centroids.
#' @param y The name of the column that contains y coordinates of bin centroids.
#'
#' @return A triangular object representing the triangulated bin centroids.
#' @importFrom interp tri.mesh
#' @importFrom rlang sym as_string
#'
#' @examples
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
#' tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#'
#' @export
tri_bin_centroids <- function(hex_df, x, y){
  tr1 <- tri.mesh(hex_df[[rlang::as_string(rlang::sym(x))]],
                  hex_df[[rlang::as_string(rlang::sym(y))]])

  # Create a list to store the triangulation object and the counts
  result <- list(
    trimesh_object = tr1,
    bin_counts = hex_df[["std_counts"]]
  )

  return(result)
}


#' Generate edge information
#'
#' This function generates edge information from a given triangular object,
#' including the coordinates of the vertices and the from-to relationships
#' between the vertices.
#'
#' @param tri_object The triangular object from which to generate edge information.
#' @param threshold A numeric value to filter high-density hexagons.
#'
#' @return A tibble that contains the edge information, including the from-to
#' relationships and the corresponding x and y coordinates.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter rename distinct left_join select
#' @importFrom interp triangles
#'
#' @examples
#' tr1_object <- s_curve_obj$s_curve_umap_model_tr1_object
#' gen_edges(tri_object = tr1_object)
#'
#' @export
gen_edges <- function(tri_object, threshold = 0) {
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
  filtered_edges <- tr_arcs_df |>
    left_join(tr_df |> select(ID, n_obs), by = c("from" = "ID")) |>
    rename(from_count = n_obs) |>
    left_join(tr_df |> select(ID, n_obs), by = c("to" = "ID")) |>
    rename(to_count = n_obs) |>
    filter(from_count > threshold & to_count > threshold) |>
    select(from, to) |>
    mutate(x = pmin(from, to), y = pmax(from, to)) |>
    distinct(x, y) |>
    rename(from = x, to = y)

  # Map from and to coordinates for the filtered edges
  tr_from_to_df_coord <- left_join(filtered_edges, tr_df, by = c("from" = "ID")) |>
    rename(x_from = x, y_from = y) |>
    left_join(tr_df, by = c("to" = "ID")) |>
    rename(x_to = x, y_to = y) |>
    select(from, to, x_from, y_from, x_to, y_to) # Keep only necessary columns

  ## Updated the from and to
  # Find the unique values in `from` and `to`, and sort them.
  unique_values <- sort(unique(c(tr_from_to_df_coord$from, tr_from_to_df_coord$to)))

  # Create a mapping between the old values and the new, renumbered values (starting from 1).
  value_map <- data.frame(old_value = unique_values, new_value = 1:length(unique_values))

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
#' @param tr_coord_df A tibble that contains the x and y coordinates of start and end points.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are removed.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes labs
#' @importFrom tibble tibble
#'
#' @examples
#' tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
#' vis_rmlg_mesh(tr_coord_df = tr_from_to_df)
#'
#' @export
vis_rmlg_mesh <- function(tr_coord_df) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(tr_coord_df[["x_from"]], tr_coord_df[["x_to"]]),
                          y = c(tr_coord_df[["y_from"]], tr_coord_df[["y_to"]])) |>
    distinct()

  ## Create the triangular mesh plot after removing the long edges
  tri_mesh_plot <- ggplot(tr_df, aes(x = x, y = y)) +
    geom_segment(aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                          data = tr_coord_df,
                 colour = "#33a02c") +
    geom_point(size = 1, colour = "#33a02c") +
    coord_equal() +
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
#' @param r2 The range of the original second embedding component.
#' @param q The buffer amount as proportion of data range.
#'
#' @return The number of bins along the x and y axes
#' needed to achieve a specific number of non-empty bins.
#'
#' @examples
#' scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
#' lim1 <- scurve_umap_scaled_obj$lim1
#' lim2 <- scurve_umap_scaled_obj$lim2
#' r2 <- diff(lim2)/diff(lim1)
#' find_non_empty_bins(data = s_curve_noise_umap_scaled, non_empty_bins = 5,
#' r2 = r2)
#'
#' @export
find_non_empty_bins <- function(data, non_empty_bins, r2, q = 0.1) {

  if (missing(non_empty_bins)) {
    stop("Required number of non-empty bins is not defined.")
  }

  max_bins_along_axis <- ceiling(sqrt(NROW(data)))

  ## Since having 1 bin along x or y-axis is not obvious therefore started from 2
  num_bins_x_vec <- 4:max_bins_along_axis

  ## To initialise the number of bins along the x-axis
  bin1 <- num_bins_x_vec[1]

  ## To compute the number of bins along the y-axis
  bin2 <- calc_bins_y(bin1 = bin1, r2 = r2, q = q)$bin2

  ### Generate the full grid
  hb_obj <- hex_binning(data = data, bin1 = bin1, r2 = r2, q = q)

  num_of_non_empty_bins <- hb_obj$non_bins

  i <- 1

  while (num_of_non_empty_bins < non_empty_bins) {

    i <- i + 1

    if (length(num_bins_x_vec) < i) {
      stop("There is no matching number of bins along the x and y axes
           for the required number of non empty bins.")
    }

    bin1 <- num_bins_x_vec[2]

    ## To compute the number of bins along the y-axis
    bin2 <- calc_bins_y(bin1 = bin1, r2 = r2, q = q)$bin2

    ### Generate the full grid
    hb_obj <- hex_binning(data = data, bin1 = bin1, r2 = r2, q = q)

    num_of_non_empty_bins <- hb_obj$non_bins

    if (num_of_non_empty_bins >= non_empty_bins) {
      return(list(bin1 = bin1, bin2 = bin2))
      break
    } else {
      next
    }
  }
}

