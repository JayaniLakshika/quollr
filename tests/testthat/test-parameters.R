test_that("find_benchmark_value() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
                                                 num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
  distance_df <- cal_2d_dist(tr_from_to_df)

  testthat::expect_equal(find_benchmark_value(distance_df, distance), 2.545)

  distance_df <- distance_df |> dplyr::mutate(distance = dplyr::if_else(dplyr::row_number() == 10, NA_integer_, distance))
  testthat::expect_snapshot(find_benchmark_value(distance_df, distance), error = TRUE)

  data_dist <- tibble::tibble(from = c(1, 2, 3),
                              to = c(2, 3, 4),
                              dist = c(1.5, 0.5, 3.7))
  testthat::expect_snapshot(find_benchmark_value(data_dist, dist))
})


test_that("compute_mean_density_hex() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
                                                 num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     num_bins_x = num_bins_x))

  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids,
                                                     num_bins_x = NA), error = TRUE)

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts_new = dplyr::if_else(dplyr::row_number() == 12, NA_integer_, std_counts))
  testthat::expect_snapshot(compute_mean_density_hex(df_bin_centroids = df_bin_centroids_na,
                                                     num_bins_x = num_bins_x, col_std_counts = std_counts_new), error = TRUE)

})


test_that("find_low_density_hexagons() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
                                                 num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.6666667)

  testthat::expect_snapshot(find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts"))

  testthat::expect_snapshot(find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)),
                                                      col_std_counts = "std_counts"))

  testthat::expect_snapshot(find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = NA,
                            df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts"), error = TRUE)

  df_bin_centroids_na <- df_bin_centroids |>
    dplyr::mutate(std_counts_new = dplyr::if_else(dplyr::row_number() == 12, NA_integer_, std_counts))
  testthat::expect_snapshot(find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids_na, num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts_new"),
                            error = TRUE)

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(std_counts <= 0.2222222)

  testthat::expect_snapshot(find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
                                                      df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts"))

})


test_that("extract_coord_of_shifted_hex_grid() works", {


})
