test_that("avg_highD_data() works", {

  suppressMessages(num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
  x = "UMAP1", hex_size = NA, buffer_x = NA))
  suppressMessages(num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
   y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA))
  umap_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), umap_with_hb_id)
  testthat::expect_snapshot(avg_highD_data(df_all, column_start_text = "x"))

})

test_that("compute_weights() works", {

  suppressMessages(num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  suppressMessages(num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                           num_bins_y = num_bins_y, x_start = NA,
                                           y_start = NA, buffer_x = NA, buffer_y = NA,
                                           hex_size = NA))
  umap_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))

  testthat::expect_snapshot(compute_weights(nldr_df_with_hex_id = umap_with_hb_id))

})

test_that("weighted_highD_data() works", {

  suppressMessages(num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  suppressMessages(num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                           num_bins_y = num_bins_y, x_start = NA,
                                           y_start = NA, buffer_x = NA, buffer_y = NA,
                                           hex_size = NA))
  umap_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))

  testthat::expect_snapshot(weighted_highD_data(training_data = s_curve_noise_training,
                                                nldr_df_with_hex_id = umap_with_hb_id,
                                                column_start_text = "x"))

})


test_that("show_langevitour() works", {

  suppressMessages(num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  suppressMessages(num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))

  ## Obtain the hexbin object
  suppressMessages(hb_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                      x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                      num_bins_y = num_bins_y, x_start = NA,
                                      y_start = NA, buffer_x = NA,
                                      buffer_y = NA, hex_size = NA))

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), umap_with_hb_id)
  df_bin <- avg_highD_data(df_all, column_start_text = "x")

  suppressWarnings(tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids,
                                                           x = "c_x", y = "c_y"))
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_col_vec = c("from", "to", "distance"))
  tour_widget <- show_langevitour(df = df_all, df_b = df_bin, df_b_with_center_data = df_bin_centroids,
                                  benchmark_value = 0.75, distance = distance_df,
                                  distance_col = "distance",
                                  use_default_benchmark_val = FALSE, column_start_text = "x")

  # Test if the output is an HTML widget object
  #testthat::expect_type(tour_widget, "list")
  testthat::expect_s3_class(tour_widget, "langevitour")

  df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

  suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
  tr_from_to_df_n <- generate_edge_info(triangular_object = tr1_object_n)

  distance_df_n <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df_n, start_x = "x_from",
                               start_y = "y_from", end_x = "x_to", end_y = "y_to",
                               select_col_vec = c("from", "to", "distance"))

  tour_widget_n <- show_langevitour(df = df_all, df_b = df_bin, df_b_with_center_data = df_bin_mean,
                                  benchmark_value = 0.75, distance = distance_df_n,
                                  distance_col = "distance",
                                  use_default_benchmark_val = FALSE, column_start_text = "x")



  # Test if the output is an HTML widget object
  #testthat::expect_type(tour_widget, "list")
  testthat::expect_s3_class(tour_widget_n, "langevitour")

})
