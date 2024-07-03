test_that("avg_highd_data() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 4, r2 = r2)

  umap_with_hb_id <- hb_obj$data_hb_id
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), umap_with_hb_id)
  testthat::expect_snapshot(avg_highd_data(data = df_all, col_start = "x"))

})


test_that("show_langevitour() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 4, r2 = r2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df) |>
    dplyr::filter(drop_empty == FALSE)

  umap_with_hb_id <- hb_obj$data_hb_id
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
                                  benchmark_value = 1.168, distance = distance_df,
                                  distance_col = "distance",
                                  use_default_benchmark_val = FALSE,
                                  col_start = "x")

  # Test if the output is an HTML widget object
  #testthat::expect_type(tour_widget, "list")
  testthat::expect_s3_class(tour_widget, "langevitour")

})
