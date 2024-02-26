#' Generate Full Grid Centroids Coordinates
#'
#' This function generates all possible centroids in the full grid based on 2D embeddings.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param num_bins_x Number of bins along the x-axis.
#' @param num_bins_y Number of bins along the y-axis.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#'
#' @return A list contains hexIDs, x and y coordinates (hexID, c_x, c_y respectively) of all hexagon bin centroids.
#' @importFrom rlang sym as_string
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#'
#' @export
generate_full_grid_centroids <- function(nldr_df, x = "UMAP1", y = "UMAP2",
                                         num_bins_x, num_bins_y, x_start = NA,
                                         y_start = NA, buffer_x = NA,
                                         buffer_y = NA, hex_size = NA){

  ## hex size is not provided
  if (is.na(hex_size)) {
    ## To compute the diameter of the hexagon
    hex_size <- 0.2
    message(paste0("Hex size is set to ", hex_size, "."))

  }

  ## If number of bins along the x-axis is not given
  if (is.na(num_bins_x)) {
    ## compute the number of bins along the x-axis
    num_bins_x <- calculate_effective_x_bins(nldr_df, x = "UMAP1",
                                             hex_size = hex_size,
                                             buffer_x = buffer_x)


  }

  ## If number of bins along the y-axis is not given
  if (is.na(num_bins_y)) {
    num_bins_y <- calculate_effective_y_bins(nldr_df, y = "UMAP2",
                                             hex_size = hex_size,
                                             buffer_y = buffer_y)

  }


  ## If x_start and y_start not define
  if (is.na(x_start)) {

    # Define starting point
    x_start <- min(nldr_df[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size/2)

    message(paste0("x_start is set to ", x_start, "."))

  } else {
    max_x_start <- min(nldr_df[[rlang::as_string(rlang::sym(x))]]) + (sqrt(3) * hex_size)
    min_x_start <- min(nldr_df[[rlang::as_string(rlang::sym(x))]]) - (sqrt(3) * hex_size)

    if ((x_start < min_x_start) | (x_start > max_x_start)){
      stop(paste0("x_start value is not compatible.
                  Need to use a value betweeen ", min_x_start," and ", max_x_start,"."))

    }

  }

  if (is.na(y_start)) {
    # Define starting point
    y_start <- min(nldr_df[[rlang::as_string(rlang::sym(y))]]) - (1.5 * hex_size/2)

    message(paste0("y_start is set to ", y_start, "."))


  } else {

    max_y_start <- min(nldr_df[[rlang::as_string(rlang::sym(x))]]) + (1.5 * hex_size)
    min_y_start <- min(nldr_df[[rlang::as_string(rlang::sym(x))]]) - (1.5 * hex_size)

    if ((y_start < min_y_start) | (y_start > max_y_start)){
      stop(paste0("y_start value is not compatible.
                  Need to use a value betweeen ", min_y_start," and ", max_y_start,"."))

    }



  }


  # Calculate horizontal and vertical spacing
  horizontal_spacing <- sqrt(3) * hex_size
  vertical_spacing <- 1.5 * hex_size

  # Initialize vector to store hexgon centroid coordinates
  c_x <- numeric(0)
  c_y <- numeric(0)

  # Generate hexagon grid
  for (i in 1:num_bins_y) {
    for (j in 1:num_bins_x) {

      if (i == 1) {
        ## For the first hexbin along the y-axis
        y <- y_start

        if (j == 1) {
          ## For the first hexbin along the x-axis
          x <- x_start

        } else {

          ## For the bins along the x-axis except the first one
          x <- x_start + (j - 1) * horizontal_spacing
          if (i %% 2 == 0) {  # Adjust for even rows
            x <- x + horizontal_spacing / 2
          }

        }

      } else {

        ## For the bins along the x and y axes except the first ones
        x <- x_start + (j - 1) * horizontal_spacing
        y <- y_start + (i - 1) * vertical_spacing
        if (i %% 2 == 0) {  # Adjust for even rows
          x <- x + horizontal_spacing / 2
        }

      }

      c_x <- append(c_x, x)
      c_y <- append(c_y, y)

    }
  }

  ## To generate hexIDs
  hexID <- 1:length(c_x)

  return(list(hexID = hexID, c_x = c_x, c_y = c_y))

}

#' Generate Hexagonal Coordinates
#'
#' This function generates the coordinates of hexagons after passing hexagonal centroids.
#'
#' @param all_centroids_df The dataset with all hexbin ID and centroid coordinates.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#'
#' @return A list contains polygon id, x and y coordinates (hex_poly_id, x, and y respectively) of hexagons.
#'
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' gen_hex_coordinates(all_centroids_df, hex_size = NA)
#'
#' @export
gen_hex_coordinates <- function(all_centroids_df, hex_size = NA){

  ## hex size is not provided
  if (is.na(hex_size)) {

    hex_size <- 0.2
    message(paste0("Hex size is set to ", hex_size, "."))

  }

  ## Obtain centroid info
  hex_ids <- all_centroids_df$hexID
  c_x_vec <- all_centroids_df$c_x
  c_y_vec <- all_centroids_df$c_y

  ## Compute the distance for hexagonal coordinates from the centroids
  if ((length(unique(c_x_vec)) == 1) || (length(unique(c_y_vec)) == 1)) {

    ## If there is only one hexagon along the x and y axis
    dx <- 2 * hex_size
    dy <- sqrt(3) * hex_size

  } else {

    ## If there is only more than one hexagon along the x and y axis
    dx <- (c_x_vec[2] - c_x_vec[1])/2
    dy <- (unique(c_y_vec)[2] - unique(c_y_vec)[1])/ sqrt(3) / 2 * 1.15


  }

  ## Assign coordinates for 6 directions
  x_add_factor <- c(dx, dx, 0, -dx, -dx, 0)
  y_add_factor <- c(dy, -dy, -2 * dy, -dy, dy, 2 * dy)

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


  return(list(hex_poly_id = hex_poly_id, x = x, y = y))
}


#' Assign Data To Hexagons
#'
#' This function generates the coordinates of hexagons after passing hexagonal centroids.
#'
#' @param nldr_df A data frame containing 2D embeddings only.
#' @param centroid_df The dataset with all centroid coordinates only.
#' @param type_NLDR The type of non-linear dimensionality reduction (NLDR) used. Default is "UMAP".
#'
#' @return A list contains 2D embeddings and respective hexagon ID (emb_1, emb_2, and hb_id respectively).
#'
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id, centroid_df = all_centroids_df)
#'
#' @export
assign_data <- function(nldr_df, centroid_df, type_NLDR = "UMAP") {

  ## To assign names for 2D embeddings
  embed_names <- paste0(type_NLDR, 1:2)

  ## To remove hexID and prepare for distance computation
  all_centroids_df <- centroid_df[, c(2, 3)]

  ## Convert to matrix
  matrix_nldr <- as.matrix(nldr_df)
  centroid_matrix <- as.matrix(all_centroids_df)

  ## Compute distances between nldr coordinates and hex bin centroids
  dist_df <- proxy::dist(matrix_nldr, centroid_matrix, method = "Euclidean")

  ## Columns that gives minimum distances
  min_column <- apply(dist_df, 1, which.min)

  emd_1 <- nldr_df[[embed_names[1]]]
  emd_2 <- nldr_df[[embed_names[2]]]
  hb_id <- centroid_df$hexID[min_column]

  ## Create a list
  assign_data_obj <- list(emd_1 = emd_1, emd_2 = emd_2, hb_id = hb_id)

  ## Rename elements in the list
  names(assign_data_obj) <- c(paste0(type_NLDR, 1:2), "hb_id")

  return(assign_data_obj)

}

#' Compute Standardize Counts in Hexagons
#'
#' This function computes the standardize number of points within each hexagon.
#'
#' @param nldr_df_with_hex_id A data frame with 2D embeddings and hexagonal bin IDs.
#'
#' @return A list that contains hexagon IDs and the corresponding standardize counts.
#' @importFrom dplyr count
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#'
#' @export
compute_std_counts <- function(nldr_df_with_hex_id) {

  ## Group by hexagon IDs
  df_with_std_counts <- nldr_df_with_hex_id |>
    dplyr::count(hb_id)

  ## To compute standardize counts
  std_counts <- df_with_std_counts$n/max(df_with_std_counts$n)

  return(list(hb_id = df_with_std_counts$hb_id, std_counts = std_counts))

}

#' Find Points in Hexagonal Bins
#'
#' This function maps points to their corresponding hexagonal bins based on the provided data frames.
#'
#' @param nldr_data_with_hb_id A data frame with 2D embedding data, ID and hexagonal bin IDs.
#'
#' @return A data frame with hexagonal bin IDs and the corresponding points.
#' @importFrom dplyr filter pull
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y, x_start = NA,
#'                                              y_start = NA, buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' umap_with_hb_id <- umap_with_hb_id |> dplyr::mutate(ID = s_curve_noise_umap_scaled$ID)
#' find_pts_in_hexbins(nldr_data_with_hb_id = umap_with_hb_id)
#'
#' @export
find_pts_in_hexbins <- function(nldr_data_with_hb_id) {

  ## A vector to store points info
  pts_list <- list()
  hexID <- integer(0)

  hexID_vec <- unique(nldr_data_with_hb_id$hb_id)

  for (hb_id in hexID_vec) {

    ## Filter a hexagon and find the point within that hexagon
    pts_vec <- nldr_data_with_hb_id |>
      dplyr::filter(hb_id == hb_id) |>
      dplyr::pull(ID) |>
      list()

    ## Rename the list
    names(pts_vec) <- paste0("Points in hexID: ", hb_id)

    ## Store the hexagon ID with the respective points
    pts_list <- append(pts_list, pts_vec)
    hexID <- append(hexID, hb_id)

  }

  return(list(hexID = hexID, pts_list = pts_list))

}


#' Generate Hexagonal Binning Info
#'
#' This function generates a list which contains hexagonal binning information.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param num_bins_x Number of bins along the x-axis.
#' @param num_bins_y Number of bins along the y-axis.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#'
#' @return A list contains all hexagonal bin centroids (full_grid_hex_centroids),
#' hexagonal coordinates of the full grid(full_grid_hex_poly_coordinates),
#' 2D embeddings with corresponding hexagon IDs (nldr_data_with_hex_id),
#' hex bins with their corresponding standardise counts (hex_id_with_std_counts),
#' total number of hex bins(total_hex_bins"), number of non-empty hex bins (num_non_empty_bins)
#' and points within each hexagon (points_in_hex_bins).
#'
#' @importFrom dplyr select mutate
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#'
#' @export
generate_hex_binning_info <- function(nldr_df, x = "UMAP1", y = "UMAP2",
                                    num_bins_x, num_bins_y, x_start = NA,
                                    y_start = NA, buffer_x = NA,
                                    buffer_y = NA, hex_size = NA) {

  ## To generate all the centroids of the grid
  centroid_list <- generate_full_grid_centroids(nldr_df = nldr_df,
                               x = x, y = y, num_bins_x = num_bins_x,
                               num_bins_y = num_bins_y, x_start = x_start,
                               y_start = y_start, buffer_x = buffer_x,
                               buffer_y = buffer_y, hex_size = hex_size)

  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))

  ## To generate the hexagon coordinates
  all_hex_coordinates_list <- gen_hex_coordinates(all_centroids_df, hex_size = hex_size)

  nldr_df_without_id <- nldr_df |>
    dplyr::select(-ID)

  ## To find which 2D embedding assigned to which hexagon
  nldr_with_hb_id_list <- assign_data(nldr_df = nldr_df_without_id,
                                      centroid_df = all_centroids_df)

  nldr_with_hex_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))

  ## To generate standardize counts of each hexagon
  std_counts_list <- compute_std_counts(nldr_df_with_hex_id = nldr_with_hex_id)

  nldr_with_hex_id <- nldr_with_hex_id |>
    dplyr::mutate(ID = nldr_df$ID)

  ## To find which points are within each hexagon
  pints_hex_list <- find_pts_in_hexbins(nldr_data_with_hb_id = nldr_with_hex_id)

  ## To generate the object of hexagon info
  hex_bin_obj <- list(centroid_list = centroid_list,
                      all_hex_coordinates_list = all_hex_coordinates_list,
                      nldr_with_hb_id_list = nldr_with_hb_id_list,
                      std_counts_list = std_counts_list,
                      total_num_bins = NROW(all_centroids_df),
                      non_empty_bins = length(std_counts_list$hb_id),
                      pints_hex_list = pints_hex_list)

  ## Rename the list elements
  names(hex_bin_obj) <- c("full_grid_hex_centroids", "full_grid_hex_poly_coordinates",
                          "nldr_data_with_hex_id", "hex_id_with_std_counts", "total_hex_bins",
                          "num_non_empty_bins", "points_in_hex_bins")

  return(hex_bin_obj)

}


#' Extract hexagonal bin centroids coordinates and the corresponding standardize counts.
#'
#' @param centroids_df A data frame contains all hexagonal bin centroid coordinates with hexagon IDs.
#' @param counts_df A data frame contains hexagon IDs with the standardize number of points within each hexagon.
#'
#' @return A data frame contains hexagon ID, centroid coordinates, and standardize counts.
#' @importFrom dplyr arrange mutate filter
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
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
#' @return A data frame contains hexagon ID, bin mean coordinates, and standardize counts.
#' @importFrom dplyr arrange group_by summarise filter mutate
#' @importFrom tidyselect everything
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id, counts_df = counts_df)
#'
#' @export
extract_hexbin_mean <- function(nldr_df_with_hex_id, counts_df) {

  ## To arrange the hexagon IDs
  counts_df <- counts_df |>
    dplyr::arrange(hb_id)

  ## To compute hexagonal bin means
  hex_mean_df <- nldr_df_with_hex_id |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean)) |>
    dplyr::arrange(hb_id) |>
    dplyr::filter(hb_id %in% counts_df$hb_id) |>
    dplyr::mutate(std_counts = counts_df$std_counts)

  ## Rename columns
  names(hex_mean_df) <- c("hexID", "c_x", "c_y", "std_counts")

  return(hex_mean_df)
}


#' Triangulate Bin Centroids/ Bin means
#'
#' This function triangulates the bin centroids/ means using the x and y coordinates
#' provided in the input data frame and returns the triangular object.
#'
#' @param hex_bin_df The data frame containing the bin centroids/ means.
#' @param x The name of the column that contains x coordinates of bin centroids/ means.
#' @param y The name of the column that contains y coordinates of bin centroids/ means.
#'
#' @return A triangular object representing the triangulated bin centroids/ means.
#' @importFrom interp tri.mesh
#' @importFrom rlang sym as_string
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#'
#' @export
triangulate_bin_centroids <- function(hex_bin_df, x = "c_x", y = "c_y"){
  tr1 <- interp::tri.mesh(hex_bin_df[[rlang::as_string(rlang::sym(x))]],
                           hex_bin_df[[rlang::as_string(rlang::sym(y))]])
  return(tr1)
}


#' Generate Edge Information
#'
#' This function generates edge information from a given triangular object,
#' including the coordinates of the vertices and the from-to relationships between the vertices.
#'
#' @param triangular_object The triangular object from which to generate edge information.
#'
#' @return A data frame containing the edge information, including the from-to
#' relationships and the corresponding x and y coordinates.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate filter rename distinct left_join
#' @importFrom interp triangles
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#' generate_edge_info(triangular_object = tr1_object)
#'
#' @export
generate_edge_info <- function(triangular_object) {

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

  return(tr_from_to_df_coord)


}

#' Calculate 2D Distances Between Points
#'
#' This function calculates the 2D distances between pairs of points in a data frame.
#'
#' @param tr_from_to_df_coord A data frame containing columns for the x and y coordinates of start and end points.
#' @param start_x Column name for the x-coordinate of the starting point.
#' @param start_y Column name for the y-coordinate of the starting point.
#' @param end_x Column name for the x-coordinate of the ending point.
#' @param end_y Column name for the y-coordinate of the ending point.
#' @param select_col_vec A character vector specifying the columns to be selected in the resulting data frame.
#'
#' @return A data frame with columns for the starting point, ending point, and calculated distances.
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from", start_y = "y_from",
#' end_x = "x_to", end_y = "y_to", select_col_vec = c("from", "to", "distance"))
#'
#' @export
cal_2d_dist <- function(tr_from_to_df_coord, start_x = "x_from", start_y = "y_from", end_x = "x_to",
                        end_y = "y_to", select_col_vec = c("from", "to", "distance")) {
  # Calculate the 2D distances
  tr_from_to_df_coord$distance <- lapply(seq(nrow(tr_from_to_df_coord)), function(x) {
    start <- unlist(tr_from_to_df_coord[x, c(start_x, start_y)], use.names = FALSE)
    end <- unlist(tr_from_to_df_coord[x, c(end_x, end_y)], use.names = FALSE)
    sqrt(sum((start - end)^2))
  })

  # Create a data frame with the from-to relationships and distances
  tr_from_to_df_coord <- tr_from_to_df_coord |>
    dplyr::select(tidyselect::all_of(select_col_vec))

  # Convert the distances to a vector and return the data frame
  tr_from_to_df_coord$distance <- unlist(tr_from_to_df_coord$distance, use.names = FALSE)
  return(tr_from_to_df_coord)
}


#' Color Long Edges
#'
#' This function colors the long edges in a triangular mesh plot based on a benchmark value.
#'
#' @param distance_edges The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges.
#' @param tr_from_to_df_coord A data frame containing columns for the x and y coordinates of start and end points.
#' @param distance_col The column name in `distance_edges` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are colored differently.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_col_vec = c("from", "to", "distance"))
#' colour_long_edges(distance_edges = distance_df, benchmark_value = 0.75,
#' tr_from_to_df_coord = tr_from_to_df, distance_col = "distance")
#'
#' @export
colour_long_edges <- function(distance_edges, benchmark_value, tr_from_to_df_coord, distance_col) {

  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(tr_from_to_df_coord[["x_from"]], tr_from_to_df_coord[["x_to"]]),
                          y = c(tr_from_to_df_coord[["y_from"]], tr_from_to_df_coord[["y_to"]])) |>
    dplyr::distinct()

  # label small and long edges
  distance_edges <- distance_edges |>
    dplyr::mutate(type = dplyr::if_else(!!as.name(distance_col) < benchmark_value, "small_edges", "long_edges"))

  # Merge edge information with distance data
  tr_from_to_df_coord <- dplyr::inner_join(tr_from_to_df_coord, distance_edges, by = c("from", "to"))

  # Create the triangular mesh plot with colored long edges
  tri_mesh_plot <- ggplot2::ggplot(tr_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = type),
      data = tr_from_to_df_coord
    ) +
    ggplot2::geom_point(size = 1, colour = "#33a02c") +
    ggplot2::coord_equal() +
    ggplot2::scale_colour_manual(values = c("#de2d26", "#636363"))

  return(tri_mesh_plot)
}

#' Remove Long Edges from a Triangular Mesh Plot
#'
#' This function removes long edges from a triangular mesh plot based on a benchmark value.
#'
#' @param distance_edges The data frame containing the edge information.
#' @param benchmark_value The threshold value to determine long edges.
#' @param tr_from_to_df_coord A data frame containing columns for the x and y coordinates of start and end points.
#' @param distance_col The column name in `distance_edges` representing the distances.
#'
#' @return A ggplot object with the triangular mesh plot where long edges are removed.
#'
#' @importFrom dplyr distinct if_else mutate inner_join
#' @importFrom ggplot2 ggplot geom_segment geom_point coord_equal scale_colour_manual aes
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2",
#'                                              num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y,
#'                                              x_start = NA, y_start = NA,
#'                                              buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
#' centroid_df = all_centroids_df)
#' umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
#' std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
#' counts_df <- as.data.frame(do.call(cbind, std_counts_list))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#'  counts_df = counts_df)
#' tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_col_vec = c("from", "to", "distance"))
#' remove_long_edges(distance_edges = distance_df, benchmark_value = 0.75,
#' tr_from_to_df_coord = tr_from_to_df, distance_col = "distance")
#'
#' @export
remove_long_edges <- function(distance_edges, benchmark_value, tr_from_to_df_coord,
                              distance_col) {
  # Create the tibble with x and y coordinates
  tr_df <- tibble::tibble(x = c(tr_from_to_df_coord[["x_from"]], tr_from_to_df_coord[["x_to"]]),
                          y = c(tr_from_to_df_coord[["y_from"]], tr_from_to_df_coord[["y_to"]])) |>
    dplyr::distinct()

  # Filter small edges
  distance_df_small_edges <- distance_edges |>
    dplyr::filter(!!as.name(distance_col) < benchmark_value)

  # Merge edge information with distance data
  tr_from_to_df_coord <- dplyr::inner_join(tr_from_to_df_coord, distance_df_small_edges,
                                           by = c("from", "to"))

  ## Create the triangular mesh plot after removing the long edges
  tri_mesh_plot <- ggplot2::ggplot(tr_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                          data = tr_from_to_df_coord) +
    ggplot2::geom_point(size = 1, colour = "#33a02c") +
    ggplot2::coord_equal() +
    ggplot2::labs(color=NULL)
  return(tri_mesh_plot)

}

#' Find the Number of Bins Required
#'
#' This function determines the number of bins along the x and y axes
#' to obtain a specific number of non-empty bins.
#'
#' @param nldr_df_with_id A data frame containing 2D embeddings and unique ID column.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param non_empty_bins The desired number of non-empty bins.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#'
#' @return The number of bins along the x and y axes
#' needed to achieve a specific number of non-empty bins.
#'
#' @examples
#' find_non_empty_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", non_empty_bins = 10, x_start = NA,
#' y_start = NA, buffer_x = NA, buffer_y = NA, hex_size = NA)
#'
#' @export
find_non_empty_bins <- function(nldr_df_with_id, x = "UMAP1", y = "UMAP2",
                                non_empty_bins, x_start = NA, y_start = NA,
                                buffer_x = NA, buffer_y = NA, hex_size = NA) {

  if (is.na(non_empty_bins)) {
    stop("Required number of non-empty bins is not defined.")
  }

  max_bins_along_axis <- ceiling(sqrt(NROW(nldr_df_with_id)))

  ## Since having 1 bin along x or y-axis is not obvious therefore started from 2
  num_bins_comb_df <- expand.grid(num_bins_x_vec = 2:max_bins_along_axis,
                                  num_bins_y_vec = 2:max_bins_along_axis) |>
    dplyr::mutate(num_x = pmin(num_bins_x_vec, num_bins_y_vec), num_y = pmax(num_bins_x_vec, num_bins_y_vec)) |>
    dplyr::distinct(num_x, num_y)

  num_bins_x <- num_bins_comb_df$num_x[1]
  num_bins_y <- num_bins_comb_df$num_y[1]

  ### Generate the full grid
  hb_obj <- generate_hex_binning_info(nldr_df = nldr_df_with_id,
                            x = x, y = y, num_bins_x = num_bins_x,
                            num_bins_y = num_bins_y, x_start = x_start,
                            y_start = y_start, buffer_x = buffer_x,
                            buffer_y = buffer_y, hex_size = hex_size)


  num_of_non_empty_bins <- hb_obj$num_non_empty_bins

  i <- 1

  while (num_of_non_empty_bins < non_empty_bins) {
    i <- i + 1

    num_bins_x <- num_bins_comb_df$num_x[i]
    num_bins_y <- num_bins_comb_df$num_y[i]

    ### Generate the full grid
    ### Generate the full grid
    hb_obj <- generate_hex_binning_info(nldr_df = nldr_df_with_id,
                                        x = x, y = y, num_bins_x = num_bins_x,
                                        num_bins_y = num_bins_y, x_start = x_start,
                                        y_start = y_start, buffer_x = buffer_x,
                                        buffer_y = buffer_y, hex_size = hex_size)


    num_of_non_empty_bins <- hb_obj$num_non_empty_bins

    if (num_of_non_empty_bins >= non_empty_bins) {
      return(list(num_bins_x = num_bins_x, num_bins_y = num_bins_y))
      break
    } else {
      next
    }
  }
}
