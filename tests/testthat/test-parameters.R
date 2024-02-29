test_that("find_lg_benchmark() works", {

  suppressMessages(model <- fit_highd_model(training_data = s_curve_noise_training,
                                            x = "UMAP1", y = "UMAP2",
                                            nldr_df_with_id = s_curve_noise_umap_scaled,
                                            col_start_2d = "UMAP", col_start_highd = "x"))
  df_bin_centroids <- model$df_bin_centroids

  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to",
                             end_y = "y_to",
                             select_vars = c("from", "to", "distance"))

  testthat::expect_equal(find_lg_benchmark(distance_edges = distance_df,
                                              distance_col = "distance"), 0.6)

  distance_df_n <- distance_df |> dplyr::mutate(distance =
                                                  dplyr::if_else(dplyr::row_number() == 10,
                                                                 NA_integer_, distance))
  testthat::expect_error(find_lg_benchmark(distance_df_n, "distance"))

})


test_that("compute_mean_density_hex() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
                             y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                         buffer_y = NA, hex_size = NA, col_start = "UMAP"))
  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)

  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     num_bins_x = num_bins_x))

  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     num_bins_x = NA), error = TRUE)

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts = dplyr::if_else(dplyr::row_number() == 5, NA_integer_, std_counts))
  testthat::expect_error(compute_mean_density_hex(df_bin_centroids = df_bin_centroids_na,
                                                     num_bins_x = num_bins_x))

})


test_that("find_low_dens_hex() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
                             y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                         buffer_y = NA, hex_size = NA, col_start = "UMAP"))
  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.43)

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                      num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = df_bin_centroids_low))

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                      num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0))))

  testthat::expect_error(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                   num_bins_x = NA,
                                                   df_bin_centroids_low = df_bin_centroids_low))

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts = dplyr::if_else(dplyr::row_number() == 8, NA_integer_, std_counts))
  testthat::expect_error(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids_na,
                                                   num_bins_x = num_bins_x,
                                                   df_bin_centroids_low = df_bin_centroids_low))

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.2222222)

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                      num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = df_bin_centroids_low))

})
