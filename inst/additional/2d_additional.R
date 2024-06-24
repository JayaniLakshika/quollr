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

test_that("extract_hexbin_mean() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                            x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                            y = "UMAP2", hex_size = NA, buffer_y = NA))

  ## Obtain the hexbin object
  hb_obj <- suppressMessages(generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                                       x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                                       num_bins_y = num_bins_y, x_start = NA,
                                                       y_start = NA, buffer_x = NA,
                                                       buffer_y = NA, hex_size = NA))

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))

  testthat::expect_snapshot(extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,
                                                counts_df = counts_df))

})


umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)
testthat::expect_snapshot(suppressWarnings(triangulate_bin_centroids(hex_bin_df = df_bin_mean,
                                                                     x = "c_x", y = "c_y")))

umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
testthat::expect_snapshot(generate_edge_info(triangular_object = tr1_object_n))


umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
tr_from_to_df_n <- generate_edge_info(triangular_object = tr1_object_n)

testthat::expect_snapshot(cal_2d_dist(tr_from_to_df_coord = tr_from_to_df_n, start_x = "x_from",
                                      start_y = "y_from", end_x = "x_to", end_y = "y_to",
                                      select_col_vec = c("from", "to", "distance")))

umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
tr_from_to_df_n <- generate_edge_info(triangular_object = tr1_object_n)

distance_df_n <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df_n, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_col_vec = c("from", "to", "distance"))

vdiffr::expect_doppelganger("color_long_edges basic with bin means",
                            colour_long_edges(distance_edges = distance_df_n,
                                              benchmark_value = 0.75,
                                              tr_from_to_df_coord = tr_from_to_df_n,
                                              distance_col = "distance"))

umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
tr_from_to_df_n <- generate_edge_info(triangular_object = tr1_object_n)

distance_df_n <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df_n, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_col_vec = c("from", "to", "distance"))

vdiffr::expect_doppelganger("remove_long_edges basic with bin means",
                            remove_long_edges(distance_edges = distance_df_n,
                                              benchmark_value = 0.75,
                                              tr_from_to_df_coord = tr_from_to_df_n,
                                              distance_col = "distance"))

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
