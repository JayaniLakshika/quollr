#' Generate centroid coordinate
#'
#' This function generates all possible centroids in the hexagonal grid.
#'
#' @param bin1 Number of bins along the x axis.
#' @param bin2 Number of bins along the y axis.
#' @param s1 The x-coordinate of the hexagonal grid starting point.
#' @param s2 The y-coordinate of the hexagonal grid starting point.
#' @param a1 The width of the hexagon.
#'
#' @return A tibble contains hexIDs, x and y coordinates (hexID, c_x, c_y respectively)
#' of all hexagon bin centroids.
#' @importFrom tibble tibble
#'
#' @examples
#' range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
#' num_bins_x <- 3
#' num_bins_list <- calc_bins_y(bin1 = num_bins_x, s1 = -0.1, s2 = -0.1,
#' r2 = range_umap2)
#' num_bins_y <- num_bins_list$bin2
#' width <- num_bins_list$a1
#' gen_centroids(bin1 = num_bins_x, bin2 = num_bins_y, s1 = -0.1, s2 = -0.1,
#' a1 = width)
#'
#' @export
gen_centroids <- function(bin1 = 2, bin2, s1 = -0.1, s2 = -0.1, a1){

  # If the number of bins along the y axis is missing
  if (missing(bin2)) {
    stop("Need to initialize the number of bins along the x-axis.")
  }

  # If hexagonal width is missing
  if (missing(a1)) {
    stop("Need to initialize the width of the hexagon.")
  }

  # Generate x-coordinate of centroids for odd rows
  c_x_vec_odd <- seq(s1, (bin1 - 1) * a1, by = a1)

  # Generate x-coordinate of centroids for even rows
  c_x_vec_even <- c_x_vec_odd + a1/2
  c_x_vec <- c(c_x_vec_odd, c_x_vec_even)

  # To compute vertical spacing
  a2 <- sqrt(3) * a1/2

  # Generate y-coordinate of centroids
  c_y_vec <- seq(s2, (bin2 - 1) * a2, by = a2)
  c_y <- rep(c_y_vec, each = bin1)

  ## Do the number of belongs y axis is even or odd and adjust the x-coordinates
  if ((bin2 %% 2) == 0) {

    c_x <- rep(c_x_vec, bin2/2)

  } else {

    if ((ceiling(bin2/2) %% 2) == 0) {

      c_x <- append(rep(c_x_vec, floor(bin2/2)), c_x_vec_odd)

    } else{

      c_x <- append(rep(c_x_vec, floor(bin2/2)), c_x_vec_even)

    }

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
#'
#'
#' @examples
#' range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
#' num_bins_x <- 3
#' num_bins_list <- calc_bins_y(bin1 = num_bins_x, s1 = -0.1, s2 = -0.1,
#' r2 = range_umap2)
#' num_bins_y <- num_bins_list$bin2
#' width <- num_bins_list$a1
#' all_centroids_df <- gen_centroids(bin1 = num_bins_x, bin2 = num_bins_y,
#' s1 = -0.1, s2 = -0.1, a1 = width)
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
#'
#'
#' @examples
#' range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
#' num_bins_x <- 3
#' num_bins_list <- calc_bins_y(bin1 = num_bins_x, s1 = -0.1, s2 = -0.1,
#' r2 = range_umap2)
#' num_bins_y <- num_bins_list$num_y
#' all_centroids_df <- gen_centroids(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2)
#' assign_data(data = s_curve_noise_umap_scaled, centroid_df = all_centroids_df)
#'
#' @export
assign_data <- function(data, centroid_df) {

  ## To select first and second embedding components
  select_emb <- data |>
    dplyr::select(c(1, 2))

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
#' @param data_hex_id A data frame with x and y coordinates and hexagonal bin IDs.
#'
#' @return A tibble that contains hexagon IDs and the corresponding standardize counts.
#' @importFrom dplyr count mutate
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' all_centroids_df <- gen_centroids(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2)
#' umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
#' centroid_df = all_centroids_df, col_start = "UMAP")
#' compute_std_counts(data_hex_id = umap_with_hb_id)
#'
#' @export
compute_std_counts <- function(data_hex_id) {

  ## Group by hexagon IDs
  std_df <- data_hex_id |>
    dplyr::count(hb_id) |>
    dplyr::mutate(std_counts = n/max(n))

  return(std_df)

}

#' Find points in hexagonal bins
#'
#' This function maps points to their corresponding hexagonal bins.
#'
#' @param data_hex_id A data frame with data, ID and hexagonal bin IDs.
#'
#' @return A data frame with hexagonal bin IDs and the corresponding points.
#' @importFrom dplyr filter pull
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' all_centroids_df <- gen_centroids(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2)
#' umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
#' centroid_df = all_centroids_df, col_start = "UMAP")
#' find_pts(data_hex_id = umap_with_hb_id)
#'
#' @export
find_pts <- function(data_hex_id) {

  ## A vector to store points info
  pts_list <- list()
  hexID <- integer(0)

  hexID_vec <- unique(data_hex_id$hb_id)

  for (hb_id in hexID_vec) {

    ## Filter a hexagon and find the point within that hexagon
    pts_vec <- data_hex_id |>
      dplyr::filter(hb_id == hb_id) |>
      dplyr::pull(ID) |>
      list()

    ## Rename the list
    names(pts_vec) <- paste0("Points in hexID: ", hb_id)

    ## Store the hexagon ID with the respective points
    pts_list <- append(pts_list, pts_vec)
    hexID <- append(hexID, hb_id)

  }

  pts_df <- tibble::tibble(hexID = hexID, pts_list = pts_list)

  return(pts_df)

}


#' Hexagonal binning
#'
#' This function generates a list which contains hexagonal binning information.
#'
#' @param data A tibble or data frame.
#' @param x The name of the column that contains values along the x-axis.
#' @param y The name of the column that contains values along the y-axis.
#' @param num_bins_x Number of bins along the x-axis.
#' @param num_bins_y Number of bins along the y-axis.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer
#' circle surrounding the hexagon.
#' @param col_start The text that begins the column name of x and y axes of data.
#'
#' @return A list contains all hexagonal bin centroids (centroids),
#' hexagonal coordinates of the full grid(hex_poly),
#' 2D embeddings with corresponding hexagon IDs (data_hb_id),
#' hex bins with their corresponding standardise counts (std_cts),
#' total number of hex bins(tot_bins), number of non-empty hex bins (non_bins)
#' and points within each hexagon (pts_bins).
#'
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#'
#' @export
hex_binning <- function(data, x, y, num_bins_x, num_bins_y, x_start,
                        y_start, buffer_x, buffer_y,
                        hex_size, col_start) {

  if (missing(hex_size)) {
    hex_size <- 0.2
  }

  # Calculate horizontal and vertical spacing
  hs <- sqrt(3) * hex_size
  vs <- 1.5 * hex_size

  if (missing(buffer_x)) {
    buffer_x <- round(hs * 1.5, 3)

    message(paste0("Buffer along the x-axis is set to ", buffer_x, "."))

  } else {
    if (buffer_x > round(hs * 1.5, 3)) {

      message(paste0("Buffer along the x-axis exceeds than ", hs, ".
                     Need to assign a value less than or equal to ", hs, "."))

    } else if (buffer_x <= 0) {

      message(paste0("Buffer along the x-axis is less than or equal to zero."))

    }
  }

  if (missing(buffer_y)) {
    buffer_y <- round(vs * 1.5, 3)

    message(paste0("Buffer along the y-axis is set to ", buffer_y, "."))


  } else {
    if (buffer_y > round(vs * 1.5, 3)) {

      message(paste0("Buffer along the y-axis exceeds than ", vs, ".
                     Need to assign a value less than or equal to ", vs, "."))

    } else if (buffer_y <= 0 ) {

      message(paste0("Buffer along the y-axis is less than or equal to zero."))

    }
  }

  ## If number of bins along the x-axis and/or y-axis is not given
  if (missing(num_bins_x) | missing(num_bins_y)) {
    ## compute the number of bins along the x-axis
    bin_list <- calc_bins(data = data, x = x, y = y, hex_size = hex_size,
                          buffer_x = buffer_x, buffer_y = buffer_y)
    num_bins_x <- bin_list$num_x
    num_bins_y <- bin_list$num_y
  }

  ## If x_start and y_start not define
  if (missing(x_start)) {
    # Define starting point
    x_start <- round(min(data[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size/2), 3)

    message(paste0("x_start is set to ", x_start, "."))

  } else {
    max_x_start <- min(data[[rlang::as_string(rlang::sym(x))]]) + (sqrt(3) * hex_size)
    min_x_start <- min(data[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size)

    if ((x_start < min_x_start) | (x_start > max_x_start)){
      stop(paste0("x_start value is not compatible.
                  Need to use a value betweeen ", min_x_start," and ", max_x_start,"."))

    }

  }

  if (missing(y_start)) {
    # Define starting point
    y_start <- round(min(data[[rlang::as_string(rlang::sym(y))]]) - (1.5 * hex_size/2), 3)

    message(paste0("y_start is set to ", y_start, "."))


  } else {

    max_y_start <- min(data[[rlang::as_string(rlang::sym(y))]]) + (1.5 * hex_size)
    min_y_start <- min(data[[rlang::as_string(rlang::sym(y))]]) - (1.5 * hex_size)

    if ((y_start < min_y_start) | (y_start > max_y_start)){
      stop(paste0("y_start value is not compatible.
                  Need to use a value betweeen ", min_y_start," and ", max_y_start,"."))

    }

  }


  ## To generate all the centroids of the grid
  all_centroids_df <- gen_centroids(data = data, x = x, y = y, num_bins_x = num_bins_x,
                               num_bins_y = num_bins_y, x_start = x_start,
                               y_start = y_start, buffer_x = buffer_x,
                               buffer_y = buffer_y, hex_size = hex_size)


  ## To generate the hexagon coordinates
  all_hex_coord <- gen_hex_coord(centroids_df = all_centroids_df,
                                            hex_size = hex_size)

  ## To find which 2D embedding assigned to which hexagon
  nldr_hex_id <- assign_data(data = data, centroid_df = all_centroids_df,
                                      col_start = col_start)

  ## To generate standardize counts of each hexagon
  std_df <- compute_std_counts(data_hex_id = nldr_hex_id)

  ## To find which points are within each hexagon
  pts_df <- find_pts(data_hex_id = nldr_hex_id)

  ## To generate the object of hexagon info
  hex_bin_obj <- list(centroids = all_centroids_df,
                      hex_poly = all_hex_coord,
                      data_hb_id = nldr_hex_id,
                      std_cts = std_df,
                      tot_bins = NROW(all_centroids_df),
                      non_bins = length(std_df$hb_id),
                      pts_bins = pts_df)

  return(hex_bin_obj)

}


#' Extract hexagonal bin centroids coordinates and the corresponding standardize counts.
#'
#' @param centroids_df A data frame contains all hexagonal bin centroid
#' coordinates with hexagon IDs.
#' @param counts_df A data frame contains hexagon IDs with the standardize
#' number of points within each hexagon.
#'
#' @return A tibble contains hexagon ID, centroid coordinates, and standardize counts.
#' @importFrom dplyr arrange mutate filter
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#'
#' @export
extract_hexbin_centroids <- function(centroids_df, counts_df) {

  ## To arrange the hexagon IDs
  counts_df <- counts_df |>
    dplyr::arrange(hb_id)

  ## Map the standardize counts
  centroids_df <- centroids_df |>
    dplyr::filter(hexID %in% counts_df$hb_id) |>
    dplyr::mutate(std_counts = counts_df$std_counts)

  return(centroids_df)
}

#' Extract hexagonal bin mean coordinates and the corresponding standardize counts.
#'
#' @param nldr_df_with_hex_id A data frame with 2D embeddings and hexagonal bin IDs.
#' @param counts_df A data frame contains hexagon IDs with the standardize number of points within each hexagon.
#'
#' @return A tibble contains hexagon ID, bin mean coordinates, and standardize counts.
#' @importFrom dplyr arrange group_by summarise filter mutate
#' @importFrom tidyselect everything
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
#' centroid_df = all_centroids_df, col_start = "UMAP")
#' extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id, counts_df = counts_df)
#'
#' @export
extract_hexbin_mean <- function(nldr_df_with_hex_id, counts_df) {

  ## To arrange the hexagon IDs
  counts_df <- counts_df |>
    dplyr::arrange(hb_id)

  ## To compute hexagonal bin means
  hex_mean_df <- nldr_df_with_hex_id |>
    dplyr::select(-ID) |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean)) |>
    dplyr::arrange(hb_id) |>
    dplyr::filter(hb_id %in% counts_df$hb_id) |>
    dplyr::mutate(std_counts = counts_df$std_counts)

  ## Rename columns
  names(hex_mean_df) <- c("hexID", "c_x", "c_y", "std_counts")

  return(hex_mean_df)
}

#' Extract k-mean center coordinates and the corresponding standardize counts.
#'
#' @param nldr_df A data frame with 2D embeddings.
#' @param tot_bins Total number of bins.
#'
#' @return A tibble contains clustering ID, kmean center coordinates, and standardize counts.
#' @importFrom dplyr mutate count bind_cols
#' @importFrom stats kmeans fitted
#' @importFrom tibble as_tibble
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' extract_kmean_centers(nldr_df = s_curve_noise_umap |> dplyr::select(-ID),
#' tot_bins = num_bins_x * num_bins_y)
#'
#' @export
extract_kmean_centers <- function(nldr_df, tot_bins) {

  ## To perform k-means clustering
  kmean_list <- stats::kmeans(nldr_df, centers = tot_bins)

  ## To extract centers of k-mean clusters
  k_means <- kmean_list$centers |>
    tibble::as_tibble() |>
    dplyr::mutate(hexID = seq_len(NROW(kmean_list$centers)))

  ## To compute standardized count within each cluster
  assign_data <- fitted(kmean_list) |>
    tibble::as_tibble() |>
    dplyr::mutate(hexID = kmean_list$cluster) |>
    dplyr::count(hexID) |>
    dplyr::mutate(std_counts = n/max(n))

  ## To prepare the dataset
  k_mean_df <- dplyr::bind_cols(k_means[, c(3, 1, 2)], assign_data[, 3])
  names(k_mean_df) <- c("hexID", "c_x", "c_y", "std_counts")

  return(k_mean_df)

}



#' Triangulate bin centroids
#'
#' This function triangulates the bin centroids using the x and y coordinates
#' provided in the input data frame and returns the triangular object.
#'
#' @param hex_df The data frame containing the bin centroids.
#' @param x The name of the column that contains x coordinates of bin centroids.
#' @param y The name of the column that contains y coordinates of bin centroids.
#'
#' @return A triangular object representing the triangulated bin centroids.
#' @importFrom interp tri.mesh
#' @importFrom rlang sym as_string
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#'
#' @export
tri_bin_centroids <- function(hex_df, x, y){
  tr1 <- interp::tri.mesh(hex_df[[rlang::as_string(rlang::sym(x))]],
                          hex_df[[rlang::as_string(rlang::sym(y))]])
  return(tr1)
}


#' Generate edge information
#'
#' This function generates edge information from a given triangular object,
#' including the coordinates of the vertices and the from-to relationships
#' between the vertices.
#'
#' @param tri_object The triangular object from which to generate edge information.
#'
#' @return A data frame containing the edge information, including the from-to
#' relationships and the corresponding x and y coordinates.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter rename distinct left_join
#' @importFrom interp triangles
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' gen_edges(tri_object = tr1_object)
#'
#' @export
gen_edges <- function(tri_object) {

  # Create a data frame with x and y coordinate values from the triangular object
  tr_df <- tibble::tibble(x = tri_object$x, y = tri_object$y,
                          ID = 1:length(tri_object$x))
  # Add ID numbers for joining with from and to points in tr_arcs

  # Extract the triangles from the triangular object
  trang <- interp::triangles(tri_object)
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

  return(tr_from_to_df_coord)


}

#' Calculate 2D Euclidean distances between vertices
#'
#' This function calculates the 2D distances between pairs of points in a data frame.
#'
#' @param tr_coord_df A data frame containing columns for the
#' x and y coordinates of start and end points.
#' @param start_x Column name for the x-coordinate of the starting point.
#' @param start_y Column name for the y-coordinate of the starting point.
#' @param end_x Column name for the x-coordinate of the ending point.
#' @param end_y Column name for the y-coordinate of the ending point.
#' @param select_vars A character vector specifying the columns to be
#' selected in the resulting data frame.
#'
#' @return A data frame with columns for the starting point, ending point,
#' and calculated distances.
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from", start_y = "y_from",
#' end_x = "x_to", end_y = "y_to", select_vars = c("from", "to", "distance"))
#'
#' @export
cal_2d_dist <- function(tr_coord_df, start_x, start_y, end_x, end_y,
                        select_vars) {

  # Calculate the 2D distances
  tr_coord_df$distance <- lapply(seq(nrow(tr_coord_df)), function(x) {
    start <- unlist(tr_coord_df[x, c(start_x, start_y)], use.names = FALSE)
    end <- unlist(tr_coord_df[x, c(end_x, end_y)], use.names = FALSE)
    sqrt(sum((start - end)^2))
  })

  # Create a data frame with the from-to relationships and distances
  tr_coord_df <- tr_coord_df |>
    dplyr::select(tidyselect::all_of(select_vars))

  # Convert the distances to a vector and return the data frame
  tr_coord_df$distance <- unlist(tr_coord_df$distance, use.names = FALSE)
  return(tr_coord_df)
}


#' Visualize triangular mesh with coloured long edges
#'
#' This function visualize triangular mesh with coloured long edges.
#'
#' @param distance_edges The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges.
#' @param tr_coord_df A data frame containing columns for the x and y coordinates of start and end points.
#' @param distance_col The column name in `distance_edges` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are
#' coloured differently.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_vars = c("from", "to", "distance"))
#' vis_lg_mesh(distance_edges = distance_df, benchmark_value = 0.75,
#' tr_coord_df = tr_from_to_df, distance_col = "distance")
#'
#' @export
vis_lg_mesh <- function(distance_edges, benchmark_value,
                         tr_coord_df, distance_col) {

  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(tr_coord_df[["x_from"]], tr_coord_df[["x_to"]]),
                          y = c(tr_coord_df[["y_from"]], tr_coord_df[["y_to"]])) |>
    dplyr::distinct()

  # label small and long edges
  distance_edges <- distance_edges |>
    dplyr::mutate(type = dplyr::if_else(!!as.name(distance_col) < benchmark_value,
                                        "small_edges", "long_edges"))

  # Merge edge information with distance data
  tr_coord_df <- dplyr::inner_join(tr_coord_df, distance_edges,
                                           by = c("from", "to"))

  # Create the triangular mesh plot with colored long edges
  tri_mesh_plot <- ggplot2::ggplot(tr_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = type),
      data = tr_coord_df
    ) +
    ggplot2::geom_point(size = 1, colour = "#33a02c") +
    ggplot2::coord_equal() +
    ggplot2::scale_colour_manual(values = c("#de2d26", "#636363"))

  return(tri_mesh_plot)
}

#' Visualize triangular mesh after removing the long edges
#'
#' This function visualize the triangular mesh after removing the long edges.
#'
#' @param distance_edges The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges.
#' @param tr_coord_df A data frame containing columns for the x and y coordinates of start and end points.
#' @param distance_col The column name in `distance_edges` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are removed.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = -0.1732051, y_start = -0.15, buffer_x = 0.346,
#' buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_vars = c("from", "to", "distance"))
#' vis_rmlg_mesh(distance_edges = distance_df, benchmark_value = 0.75,
#' tr_coord_df = tr_from_to_df, distance_col = "distance")
#'
#' @export
vis_rmlg_mesh <- function(distance_edges, benchmark_value, tr_coord_df,
                              distance_col) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(tr_coord_df[["x_from"]], tr_coord_df[["x_to"]]),
                          y = c(tr_coord_df[["y_from"]], tr_coord_df[["y_to"]])) |>
    dplyr::distinct()

  # Filter small edges
  distance_df_small_edges <- distance_edges |>
    dplyr::filter(!!as.name(distance_col) < benchmark_value)

  # Merge edge information with distance data
  tr_coord_df <- dplyr::inner_join(tr_coord_df, distance_df_small_edges,
                                           by = c("from", "to"))

  ## Create the triangular mesh plot after removing the long edges
  tri_mesh_plot <- ggplot2::ggplot(tr_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                          data = tr_coord_df) +
    ggplot2::geom_point(size = 1, colour = "#33a02c") +
    ggplot2::coord_equal() +
    ggplot2::labs(color=NULL)
  return(tri_mesh_plot)

}

#' Find the number of bins required to achieve required number of non-empty bins.
#'
#' This function determines the number of bins along the x and y axes
#' to obtain a specific number of non-empty bins.
#'
#' @param data A tibble or data frame.
#' @param x The name of the column that contains values along the x-axis.
#' @param y The name of the column that contains values along the y-axis.
#' @param non_empty_bins The desired number of non-empty bins.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer
#' circle surrounding the hexagon.
#' @param col_start The text that begins the column name of x and y axes of data.
#'
#' @return The number of bins along the x and y axes
#' needed to achieve a specific number of non-empty bins.
#'
#' @examples
#' find_non_empty_bins(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", non_empty_bins = 10, x_start = -0.1732051, y_start = -0.15,
#' buffer_x = 0.346, buffer_y = 0.3, hex_size = 0.2, col_start = "UMAP")
#'
#' @export
find_non_empty_bins <- function(data, x, y, non_empty_bins, x_start, y_start,
                                buffer_x, buffer_y, hex_size, col_start) {

  if (missing(hex_size)) {
    hex_size <- 0.2
  }

  # Calculate horizontal and vertical spacing
  hs <- sqrt(3) * hex_size
  vs <- 1.5 * hex_size

  if (missing(buffer_x)) {
    buffer_x <- round(hs * 1.5, 3)

    message(paste0("Buffer along the x-axis is set to ", buffer_x, "."))

  } else {
    if (buffer_x > round(hs, 3)) {

      message(paste0("Buffer along the x-axis exceeds than ", hs, ".
                     Need to assign a value less than or equal to ", hs, "."))

    } else if (buffer_x <= 0 ) {

      message(paste0("Buffer along the x-axis is less than or equal to zero."))

    }
  }

  if (missing(buffer_y)) {
    buffer_y <- round(vs * 1.5, 3)

    message(paste0("Buffer along the y-axis is set to ", buffer_y, "."))


  } else {
    if (buffer_y > round(vs, 3)) {

      message(paste0("Buffer along the y-axis exceeds than ", vs, ".
                     Need to assign a value less than or equal to ", vs, "."))

    } else if (buffer_y <= 0 ) {

      message(paste0("Buffer along the y-axis is less than or equal to zero."))

    }
  }

  ## If x_start and y_start not define
  if (missing(x_start)) {
    # Define starting point
    x_start <- round(min(data[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size/2), 3)

    message(paste0("x_start is set to ", x_start, "."))

  } else {
    max_x_start <- min(data[[rlang::as_string(rlang::sym(x))]]) + (sqrt(3) * hex_size)
    min_x_start <- min(data[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size)

    if ((x_start < min_x_start) | (x_start > max_x_start)){
      stop(paste0("x_start value is not compatible.
                  Need to use a value betweeen ", min_x_start," and ", max_x_start,"."))

    }

  }

  if (missing(y_start)) {
    # Define starting point
    y_start <- round(min(data[[rlang::as_string(rlang::sym(y))]]) - (1.5 * hex_size/2), 3)

    message(paste0("y_start is set to ", y_start, "."))


  } else {

    max_y_start <- min(data[[rlang::as_string(rlang::sym(y))]]) + (1.5 * hex_size)
    min_y_start <- min(data[[rlang::as_string(rlang::sym(y))]]) - (1.5 * hex_size)

    if ((y_start < min_y_start) | (y_start > max_y_start)){
      stop(paste0("y_start value is not compatible.
                  Need to use a value betweeen ", min_y_start," and ", max_y_start,"."))

    }

  }



  if (missing(non_empty_bins)) {
    stop("Required number of non-empty bins is not defined.")
  }

  max_bins_along_axis <- ceiling(sqrt(NROW(data)))

  ## Since having 1 bin along x or y-axis is not obvious therefore started from 2
  num_bins_comb_df <- expand.grid(num_bins_x_vec = 2:max_bins_along_axis,
                                  num_bins_y_vec = 2:max_bins_along_axis) |>
    dplyr::mutate(num_x = pmin(num_bins_x_vec, num_bins_y_vec),
                  num_y = pmax(num_bins_x_vec, num_bins_y_vec)) |>
    dplyr::distinct(num_x, num_y) |>
    dplyr::mutate(num_bins = num_x * num_y) |>
    dplyr::filter(num_bins >= non_empty_bins)

  num_bins_x <- num_bins_comb_df$num_x[1]
  num_bins_y <- num_bins_comb_df$num_y[1]

  ### Generate the full grid
  hb_obj <- hex_binning(data = data, x = x, y = y, num_bins_x = num_bins_x,
                        num_bins_y = num_bins_y, x_start = x_start,
                        y_start = y_start, buffer_x = buffer_x,
                        buffer_y = buffer_y, hex_size = hex_size,
                        col_start = col_start)


  num_of_non_empty_bins <- hb_obj$non_bins

  i <- 1

  while (num_of_non_empty_bins < non_empty_bins) {

    i <- i + 1

    if (NROW(num_bins_comb_df) < i) {
      stop("There is no matching number of bins along the x and y axes
           for the required number of non empty bins.")
    }

    num_bins_x <- num_bins_comb_df$num_x[i]
    num_bins_y <- num_bins_comb_df$num_y[i]

    ### Generate the full grid
    hb_obj <- hex_binning(data = data, x = x, y = y, num_bins_x = num_bins_x,
                          num_bins_y = num_bins_y, x_start = x_start,
                          y_start = y_start, buffer_x = buffer_x,
                          buffer_y = buffer_y, hex_size = hex_size,
                          col_start = col_start)


    num_of_non_empty_bins <- hb_obj$non_bins

    if (num_of_non_empty_bins >= non_empty_bins) {
      return(list(num_bins_x = num_bins_x, num_bins_y = num_bins_y))
      break
    } else {
      next
    }
  }
}
