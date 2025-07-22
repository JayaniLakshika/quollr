#' Generate centroid coordinate
#'
#' This function generates all possible centroids in the hexagonal grid.
#'
#' @param nldr_obj A list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data.
#' @param b1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A tibble contains hexIDs (\code{h}),
#' x and y coordinates (\code{c_x}, \code{c_y} respectively)
#' of all hexagon bin centroids.
#'
#' @examples
#' gen_centroids(nldr_obj = scurve_model_obj$nldr_obj, b1 = 5, q = 0.1)
#'
#' @export
gen_centroids <- function(nldr_obj, b1 = 5, q = 0.1){

  ## To check whether b2 greater than 2
  if (b1 < 2) {
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
  bin_obj <- calc_bins_y(nldr_obj = nldr_obj, b1 = b1, q = q)

  # To obtain the bins along the y-axis
  b2 <- bin_obj$b2

  # To obtain the width of the hexagon
  a1 <- bin_obj$a1

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  # Generate x-coordinate of centroids for odd rows
  c_x_vec_odd <- seq(s1, by = a1, length.out = b1) ## since x range is 0-1

  # Generate x-coordinate of centroids for even rows
  c_x_vec_even <- c_x_vec_odd + a1/2
  c_x_vec <- c(c_x_vec_odd, c_x_vec_even)

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  # Generate y-coordinate of centroids
  c_y_vec <- seq(s2, by = a2, length.out = b2)
  c_y <- rep(c_y_vec, each = b1)

  ## Do the number of belongs y axis is even or odd and adjust the x-coordinates
  if ((b2 %% 2) == 0) {

    c_x <- rep(c_x_vec, b2/2)

  } else {

    c_x <- append(rep(c_x_vec, floor(b2/2)), c_x_vec_odd)

  }

  centroids_data <- tibble::tibble(h = 1:length(c_x), c_x = c_x, c_y = c_y)

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
#' @return A tibble contains hexagon id (\code{h}),
#' x and y coordinates (\code{x}, \code{y}) of hexagons.
#'
#' @examples
#' width <- scurve_model_obj$hb_obj$a1
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' gen_hex_coord(centroids_data = all_centroids_df, a1 = width)
#'
#' @export
gen_hex_coord <- function(centroids_data, a1) {
  if (missing(a1)) cli::cli_abort("Need to initialize the width of the hexagon.")
  gen_hex_coord_cpp(centroids_data$h, centroids_data$c_x, centroids_data$c_y, a1)
}

#' Assign data to hexagons
#'
#' This function assigns the data to hexagons.
#'
#' @param nldr_obj A list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data.
#' @param centroids_data The dataset with centroid coordinates.
#'
#' @return A tibble contains embedding components (\code{emb1}, \code{emb2})
#' and corresponding hexagon ID (\code{h}).
#'
#' @examples
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' assign_data(nldr_obj = scurve_model_obj$nldr_obj,
#' centroids_data = all_centroids_df)
#'
#' @export
assign_data <- function(nldr_obj, centroids_data) {

  scaled_nldr_df <- nldr_obj$scaled_nldr

  # Select embedding dimensions
  matrix_nldr <- as.matrix(scaled_nldr_df[, 1:2])
  centroid_matrix <- as.matrix(centroids_data[, 2:3])

  # Use C++ function to find nearest centroid indices
  min_column <- compute_highd_dist(matrix_nldr, centroid_matrix)

  # Map to hex bin IDs
  hb_ids <- centroids_data$h[min_column]

  # Add hex bin ID column
  scaled_nldr_df$h <- hb_ids

  return(scaled_nldr_df)
}

#' Compute standardise counts in hexagons
#'
#' This function computes the standardize number of points within each hexagon.
#'
#' @param scaled_nldr_hexid A tibble that contains the scaled embedding with hexagonal bin IDs.
#'
#' @return A tibble that contains hexagon IDs (\code{h}), bin counts (\code{n_h}),
#' and standardize counts (\code{w_h}).
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
    count(h) |>
    mutate(w_h = n/sum(n)) |>
    rename(n_h = n)

  return(std_df)

}

#' Find points in hexagonal bins
#'
#' This function maps points to their corresponding hexagonal bins.
#'
#' @param scaled_nldr_hexid A tibble that contains the scaled embedding with hexagonal bin IDs.
#'
#' @return A tibble with hexagonal bin IDs (\code{h}) and the corresponding points.
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
    group_by(h) |>
    summarize(pts_list = list(ID), .groups = "drop")

  return(pts_df)

}


#' Hexagonal binning
#'
#' This function generates the hexagonal object.
#'
#' @param nldr_obj A list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data.
#' @param b1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A object that contains numeric vector that contains binwidths (\code{a1}),
#' vertical distance (\code{a2}), bins along the x and y axes respectively (\code{bins}),
#' numeric vector that contains hexagonal
#' starting point coordinates all hexagonal bin centroids (\code{centroids}),
#' hexagonal coordinates of the full grid (\code{hex_poly}),
#' embedding components with their corresponding hexagon IDs (\code{data_hb_id}),
#' hex bins with their corresponding standardise counts (\code{std_cts}),
#' total number of hex bins (\code{b}), number of non-empty hex bins (\code{m})
#' and points within each hexagon (\code{pts_bins}).
#'
#'
#' @examples
#' hex_binning(nldr_obj = scurve_model_obj$nldr_obj, b1 = 5, q = 0.1)
#'
#' @export
hex_binning <- function(nldr_obj, b1 = 5, q = 0.1) {

  scaled_nldr <- nldr_obj$scaled_nldr

  ## To compute the range
  lim1 <- nldr_obj$lim1
  lim2 <- nldr_obj$lim2
  r2 <- diff(lim2)/diff(lim1)

  ## To compute the number of bins along the y-axis
  bin_obj <- calc_bins_y(nldr_obj = nldr_obj, b1 = b1, q = q)
  b2 <- bin_obj$b2

  ## To obtain the width of the hexagon
  a1 <- bin_obj$a1

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  ## To generate all the centroids of the grid
  all_centroids_df <- gen_centroids(nldr_obj = nldr_obj, b1 = b1, q = q)

  ## To generate the hexagon coordinates
  all_hex_coord <- gen_hex_coord(centroids_data = all_centroids_df, a1 = a1)

  ## To find which 2-D embedding assigned to which hexagon
  nldr_hex_id <- assign_data(nldr_obj = nldr_obj, centroids_data = all_centroids_df)

  ## To generate standardize counts of each hexagon
  std_df <- compute_std_counts(scaled_nldr_hexid = nldr_hex_id)

  ## To find which points are within each hexagon
  pts_df <- find_pts(scaled_nldr_hexid = nldr_hex_id)

  ## To generate the object of hexagon info
  hex_bin_obj <- list(a1 = a1,
                      a2 = a2,
                      bins = c(b1, b2),
                      start_point = c(s1, s2),
                      centroids = all_centroids_df,
                      hex_poly = all_hex_coord,
                      data_hb_id = nldr_hex_id,
                      std_cts = std_df,
                      b = NROW(all_centroids_df),
                      m = length(std_df$h),
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
#' @return A tibble contains hexagon ID (\code{h}), centroid coordinates (\code{c_x}, \code{c_y}),
#' bin counts (\code{n_h}), and standardise counts (\code{w_h}).
#'
#' @examples
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' counts_data <- scurve_model_obj$hb_obj$std_cts
#' extract_hexbin_centroids(centroids_data = all_centroids_df,
#' counts_data = counts_data)
#'
#' @export
extract_hexbin_centroids <- function(centroids_data, counts_data) {

  # Fast merge without unnecessary sorting
  merged_data <- merge(centroids_data, counts_data, by = "h", all = TRUE)

  # Replace NA values with 0 for both columns
  merged_data$w_h[is.na(merged_data$w_h)] <- 0
  merged_data$n_h[is.na(merged_data$n_h)] <- 0

  return(merged_data)
}

#' Extract hexagonal bin mean coordinates and the corresponding standardize counts.
#'
#' @param data_hb A tibble with embedding components and hexagonal bin IDs.
#' @param counts_data A tibble that contains hexagon IDs with the standardise
#' number of points within each hexagon.
#' @param centroids_data A tibble that contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#'
#' @return A tibble contains hexagon ID (\code{h}), bin means (\code{c_x}, \code{c_y}),
#' bin counts (\code{n_h}), and standardise counts (\code{w_h}).
#'
#' @examples
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' counts_data <- scurve_model_obj$hb_obj$std_cts
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' extract_hexbin_mean(data_hb = umap_with_hb_id, counts_data = counts_data,
#' centroids_data = all_centroids_df)
#'
#' @export
extract_hexbin_mean <- function(data_hb, counts_data, centroids_data) {

  ## To arrange the hexagon IDs
  counts_data <- counts_data |>
    dplyr::arrange(h)

  ## To join the datasets
  centroids_data <- dplyr::full_join(centroids_data, counts_data, by = "h") |>
    dplyr::select(-c(c_x, c_y))

  ## To compute hexagonal bin means
  hex_mean_df <- data_hb |>
    dplyr::select(-ID) |>
    dplyr::group_by(h) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean)) |>
    dplyr::arrange(h)

  ## Rename columns
  names(hex_mean_df) <- c("h", "c_x", "c_y")

  centroids_data <- dplyr::right_join(centroids_data, hex_mean_df, by = "h") |>
    dplyr::select(h, c_x, c_y, n_h, w_h)

  # Replace NA values with 0 for both columns
  centroids_data$w_h[is.na(centroids_data$w_h)] <- 0
  centroids_data$n_h[is.na(centroids_data$n_h)] <- 0

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
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' counts_data <- scurve_model_obj$hb_obj$std_cts
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' df_bin_centroids <- extract_hexbin_mean(data_hb = umap_with_hb_id,
#' counts_data = counts_data, centroids_data = all_centroids_df)
#' tri_bin_centroids(centroids_data = df_bin_centroids)
#'
#' @export
tri_bin_centroids <- function(centroids_data){
  tr <- tri.mesh(centroids_data[["c_x"]], centroids_data[["c_y"]])

  # Create a list to store the triangulation object and the counts
  result <- list(
    trimesh_object = tr,
    n_h = centroids_data[["n_h"]]
  )

  return(result)
}

#' Calculate 2-D Euclidean distances between vertices
#'
#' This function calculates the 2-D distances between pairs of points in a data frame.
#'
#' @param trimesh_data A tibble that contains the x and y coordinates of start
#' and end points.
#' @param select_vars selected columns in the resulting data frame.
#'
#' @return A tibble with columns for the starting point, ending point,
#' and calculated distances.
#' @importFrom dplyr select mutate
#' @importFrom tidyselect all_of
#'
#' @examples
#' tr_from_to_df <- scurve_model_obj$trimesh_data
#' calc_2d_dist(trimesh_data = tr_from_to_df)
#'
#' @export
calc_2d_dist <- function(trimesh_data,
                         select_vars = c("from", "to", "x_from", "y_from", "x_to",
                                         "y_to", "from_count", "to_count", "distance")) {

  # Calculate distances using Rcpp
  dist <- calc_2d_dist_cpp(
    trimesh_data$x_from,
    trimesh_data$y_from,
    trimesh_data$x_to,
    trimesh_data$y_to
  )

  # Add distances and return selected columns
  trimesh_data <- trimesh_data |>
    dplyr::mutate(distance = dist) |>
    dplyr::select(dplyr::all_of(select_vars))

  return(trimesh_data)
}

#' Generate edge information
#'
#' This function generates edge information from a given triangular object,
#' including the coordinates of the vertices and the from-to relationships
#' between the vertices.
#'
#' @param tri_object The triangular object from which to generate edge information.
#' @param a1 A numeric value for bin width.
#'
#' @return A tibble that contains the edge information, including the from-to
#' relationships and the corresponding x and y coordinates.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter rename distinct left_join select
#' @importFrom interp triangles
#'
#' @examples
#' all_centroids_df <- scurve_model_obj$hb_obj$centroids
#' counts_data <- scurve_model_obj$hb_obj$std_cts
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' df_bin_centroids <- extract_hexbin_centroids(counts_data = counts_data,
#' centroids_data = all_centroids_df)
#' tr1_object <- tri_bin_centroids(centroids_data = df_bin_centroids)
#' gen_edges(tri_object = tr1_object, a1 = scurve_model_obj$hb_obj$a1)
#'
#' @export
gen_edges <- function(tri_object, a1) { #centroids_data
  tri <- tri_object$trimesh_object
  counts <- tri_object$n_h

  # Create tr_df with coordinates and bin counts
  n <- length(tri$x)
  tr_df <- data.frame(
    ID = seq_len(n),
    x = tri$x,
    y = tri$y,
    n_obs = counts
  )

  # Get triangles
  trang <- triangles(tri)

  # Build edges: from-to
  tr_arcs_df <- data.frame(
    from = c(trang[, 1], trang[, 1], trang[, 2]),
    to = c(trang[, 2], trang[, 3], trang[, 3])
  )

  # Canonical edge representation: lower ID first
  from <- pmin(tr_arcs_df$from, tr_arcs_df$to)
  to   <- pmax(tr_arcs_df$from, tr_arcs_df$to)
  edges_all <- unique(data.frame(from, to))

  # Vectorized coordinate and count lookup
  x_from <- tri$x[edges_all$from]
  y_from <- tri$y[edges_all$from]
  x_to   <- tri$x[edges_all$to]
  y_to   <- tri$y[edges_all$to]
  from_count <- counts[edges_all$from]
  to_count   <- counts[edges_all$to]

  # Compute Euclidean distance directly (vectorized)
  dx <- x_from - x_to
  dy <- y_from - y_to
  dist <- sqrt(dx^2 + dy^2)

  # Keep only close-enough edges
  max_dist <- sqrt(a1^2 + (sqrt(3) * a1 / 2)^2)
  keep <- dist <= max_dist

  # Final output
  edge_data <- tibble::tibble(
    from = edges_all$from[keep],
    to = edges_all$to[keep],
    x_from = x_from[keep],
    y_from = y_from[keep],
    x_to = x_to[keep],
    y_to = y_to[keep],
    from_count = from_count[keep],
    to_count = to_count[keep]
  )

  return(edge_data)
}

#' Update from and to values in trimesh data
#'
#' This function update the from and to indexes.
#'
#' @param trimesh_data A tibble that contains wireframe data.
#'
#' @return A tibble that contains the updated edge information.
#'
#' @importFrom dplyr left_join mutate select
#'
#' @examples
#' tr_from_to_df <- scurve_model_obj$trimesh_data
#' update_trimesh_index(trimesh_data = tr_from_to_df)
#'
#' @export
update_trimesh_index <- function(trimesh_data) {

  # Create unique sorted vector of node IDs
  unique_values <- sort(unique(c(trimesh_data$from, trimesh_data$to)))

  # Create named vector mapping old IDs to new ones
  id_map <- setNames(seq_along(unique_values), unique_values)

  # Vectorized replacement using the map
  trimesh_data$from <- id_map[as.character(trimesh_data$from)]
  trimesh_data$to   <- id_map[as.character(trimesh_data$to)]

  return(trimesh_data)

}

#' Find the number of bins required to achieve required number of non-empty bins.
#'
#' This function determines the number of bins along the x and y axes
#' to obtain a specific number of non-empty bins.
#'
#' @param nldr_obj A list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data.
#' @param m The desired number of non-empty bins.
#' @param q The buffer amount as proportion of data range.
#'
#' @return The number of bins along the x and y axes
#' needed to achieve a specific number of non-empty bins.
#'
#' @examples
#' find_non_empty_bins(nldr_obj = scurve_model_obj$nldr_obj, m = 5)
#'
#' @export
find_non_empty_bins <- function(nldr_obj, m = 2, q = 0.1) {

  ## To check whether q is between a specific range
  if (!dplyr::between(q, 0.05, 0.2)) {
    cli::cli_abort("The buffer should be within 0.05 and 0.2.")
  }

  scaled_nldr_data <- nldr_obj$scaled_nldr

  max_bins_along_axis <- ceiling(sqrt(NROW(scaled_nldr_data)))

  ## Since having 1 bin along x or y-axis is not obvious therefore started from 2
  num_bins_x_vec <- 2:max_bins_along_axis

  ## To initialise the number of bins along the x-axis
  b1 <- num_bins_x_vec[1]

  ### Generate the full grid
  hb_obj <- hex_binning(nldr_obj = nldr_obj, b1 = b1, q = q)

  ## To compute the number of bins along the y-axis
  b2 <- hb_obj$bins[2]

  num_of_non_empty_bins <- hb_obj$m

  i <- 1

  while (num_of_non_empty_bins < m) {

    i <- i + 1

    if (length(num_bins_x_vec) < i) {
      stop("There is no matching number of bins along the x and y axes
           for the required number of non empty bins.")
    }

    b1 <- num_bins_x_vec[2]

    ### Generate the full grid
    hb_obj <- hex_binning(nldr_obj = nldr_obj, b1 = b1, q = q)

    ## To compute the number of bins along the y-axis
    b2 <- hb_obj$bins[2]

    num_of_non_empty_bins <- hb_obj$m

    if (num_of_non_empty_bins >= m) {
      return(list(b1 = b1, b2 = b2))
      break
    } else {
      next
    }
  }
}

