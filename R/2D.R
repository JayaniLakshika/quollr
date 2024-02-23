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
#'
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

    if ((y_start < min_y_start) | (xy_start > max_y_start)){
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

        y <- y_start

        if (j == 1) {

          x <- x_start

        } else {

          x <- x_start + (j - 1) * horizontal_spacing
          if (i %% 2 == 0) {  # Adjust for even rows
            x <- x + horizontal_spacing / 2
          }

        }

      } else {

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

  hex_ids <- all_centroids_df$hexID
  c_x_vec <- all_centroids_df$c_x
  c_y_vec <- all_centroids_df$c_y

  ## Compute the distance for hexagonal coordinates from the centroids
  if ((length(unique(c_x_vec)) == 1) || (length(unique(c_y_vec)) == 1)) {

    dx <- 2 * hex_size
    dy <- sqrt(3) * hex_size

  } else {

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

    hexID_rep <- rep(hex_ids[hb_id], each = 6)
    c_x_rep <- rep(c_x_vec[hb_id], each = 6)
    c_y_rep <- rep(c_y_vec[hb_id], each = 6)

    x_spec <- c_x_rep + x_add_factor
    y_spec <- c_y_rep + y_add_factor

    x <- append(x, x_spec)
    y <- append(x, y_spec)
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
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list[c(2, 3)]))
#' s_curve_noise_umap_scaled <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' assign_data(nldr_df = s_curve_noise_umap_scaled, centroid_df = all_centroids_df)
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

  return(list(emd_1 = emd_1, emd_2 = emd_2, hb_id = hb_id))

}

#' Find Points in Hexagonal Bins
#'
#' This function maps points to their corresponding hexagonal bins based on the provided data frames.
#'
#' @param full_grid_with_hexbin_id A data frame with hexagonal bin IDs and coordinates.
#' @param nldr_data_with_hb_id A data frame with 2D embedding data, ID and hexagonal bin IDs.
#'
#' @return A data frame with hexagonal bin IDs and the corresponding points.
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'                                         y = "UMAP2", hex_size = NA, buffer_y = NA)
#' centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
#'                                              x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#'                                              num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#'                                              buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
#' s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
#' nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id, centroid_df = all_centroids_df)
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

    ## Store the hexagon ID with the respective points
    pts_list <- append(pts_list, pts_vec)
    hexID <- append(hexID, hb_id)

  }

  return(list(hexID = hexID, pts_list = pts_list))

}


#' Triangulate Bin Centroids
#'
#' This function triangulates the bin centroids/ means using the x and y coordinates
#' provided in the input data frame and returns the triangular object.
#'
#' @param hex_bin_df The data frame containing the bin centroids/ means.
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
#' triangulate_bin_centroids(df_bin_centroids, "x", "y")
#'
#' @importFrom tripack tri.mesh
#' @export
triangulate_bin_centroids <- function(hex_bin_df, x = "x", y = "y"){
  tr1 <- tripack::tri.mesh(hex_bin_df[[rlang::as_string(rlang::ensym(x))]],
                           hex_bin_df[[rlang::as_string(rlang::ensym(y))]])
  return(tr1)
}
