test_that("avg_highd_data() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
                             y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                         buffer_y = NA, hex_size = NA, col_start = "UMAP"))
  umap_with_hb_id <- hb_obj$data_hb_id
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), umap_with_hb_id)
  testthat::expect_snapshot(avg_highd_data(data = df_all, col_start = "x"))

})


test_that("show_langevitour() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
                             y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = NA,
                                         y_start = NA, buffer_x = NA,
                                         buffer_y = NA, hex_size = NA,
                                         col_start = "UMAP"))

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$data_hb_id))
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), umap_with_hb_id)
  df_bin <- avg_highd_data(data = df_all, col_start = "x")

  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to",
                             end_y = "y_to",
                             select_vars = c("from", "to", "distance"))
  tour_widget <- show_langevitour(df = df_all, df_b = df_bin,
                                  df_b_with_center_data = df_bin_centroids,
                                  benchmark_value = 0.75, distance = distance_df,
                                  distance_col = "distance",
                                  use_default_benchmark_val = FALSE,
                                  col_start = "x")

  # Test if the output is an HTML widget object
  #testthat::expect_type(tour_widget, "list")
  testthat::expect_s3_class(tour_widget, "langevitour")

})
