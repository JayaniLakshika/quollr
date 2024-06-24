test_that("find_lg_benchmark() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           bin1 = 3, r2 = r2, col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids

  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to",
                             end_y = "y_to",
                             select_vars = c("from", "to", "distance"))

  testthat::expect_equal(find_lg_benchmark(distance_edges = distance_df,
                                              distance_col = "distance"), 0.607)

  distance_df_n <- distance_df |> dplyr::add_row(from = 5, to = 6,
                                                distance = NA_integer_)
  testthat::expect_error(find_lg_benchmark(distance_df_n, "distance"))

})


test_that("compute_mean_density_hex() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, r2 = r2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df) |>
    dplyr::filter(drop_empty == FALSE)

  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     bin1 = 3))

  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     bin1 = 3))

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts = dplyr::if_else(dplyr::row_number() == 5, NA_integer_, std_counts))
  testthat::expect_error(compute_mean_density_hex(df_bin_centroids = df_bin_centroids_na,
                                                  bin1 = 3))

})


test_that("find_low_dens_hex() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, r2 = r2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df) |>
    dplyr::filter(drop_empty == FALSE)

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.3)

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                      bin1 = 3,
                                                      df_bin_centroids_low = df_bin_centroids_low))

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                              bin1 = 3,
                                              df_bin_centroids_low =
                                                data.frame(matrix(nrow = 0, ncol = 0))))

  testthat::expect_error(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                   bin1 = NA,
                                                   df_bin_centroids_low = df_bin_centroids_low))

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts = dplyr::if_else(dplyr::row_number() == 3, NA_integer_, std_counts))
  testthat::expect_error(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids_na,
                                                   bin1 = 3,
                                                   df_bin_centroids_low = df_bin_centroids_low))

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.2222222)

  testthat::expect_snapshot(find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                                      bin1 = 3,
                                                      df_bin_centroids_low = df_bin_centroids_low))

})
