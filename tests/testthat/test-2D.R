test_that("generate_full_grid_centroids() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
  x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
   y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA))
  testthat::expect_snapshot(centroid_list)

})

test_that("gen_hex_coordinates() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                                x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                                num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                                buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  suppressMessages(all_hex_coordinates_list <- gen_hex_coordinates(all_centroids_df, hex_size = NA))
  testthat::expect_snapshot(all_hex_coordinates_list)

})

test_that("assign_data() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                                x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                                num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                                buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id, centroid_df = all_centroids_df)
  testthat::expect_snapshot(nldr_with_hb_id_list)

})

test_that("compute_std_counts() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                                x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                                num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                                buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id, centroid_df = all_centroids_df)
  umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
  std_counst_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
  testthat::expect_snapshot(std_counst_list)

})

test_that("find_pts_in_hexbins() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                          x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                          y = "UMAP2", hex_size = NA, buffer_y = NA))
  suppressMessages(centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                               x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                               num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
                                               buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id, centroid_df = all_centroids_df)
  umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
  umap_with_hb_id <- umap_with_hb_id |> dplyr::mutate(ID = s_curve_noise_umap_scaled$ID)

  testthat::expect_snapshot(find_pts_in_hexbins(nldr_data_with_hb_id = umap_with_hb_id))

})

test_that("find_non_empty_bins() works", {

  testthat::expect_snapshot(suppressMessages(find_non_empty_bins(nldr_df = s_curve_noise_umap_scaled,
                                      x = "UMAP1", y = "UMAP2", non_empty_bins = 10,
                                      x_start = NA, y_start = NA, buffer_x = NA,
                                      buffer_y = NA, hex_size = NA)))

  testthat::expect_error(find_non_empty_bins(nldr_df = s_curve_noise_umap_scaled,
                                   x = "UMAP1", y = "UMAP2", non_empty_bins = NA,
                                   x_start = NA, y_start = NA, buffer_x = NA,
                                   buffer_y = NA, hex_size = NA))

})

test_that("generate_hex_binning_info() works", {

  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))

  testthat::expect_snapshot(suppressMessages(generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                            x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                            num_bins_y = num_bins_y, x_start = NA,
                                            y_start = NA, buffer_x = NA,
                                            buffer_y = NA, hex_size = NA)))

})


test_that("extract_hexbin_centroids() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))

  testthat::expect_snapshot(extract_hexbin_centroids(centroids_df = all_centroids_df,
                                                     counts_df = counts_df))

})

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

test_that("triangulate_bin_centroids() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  testthat::expect_snapshot(suppressWarnings(triangulate_bin_centroids(hex_bin_df = df_bin_centroids,
                                                             x = "c_x", y = "c_y")))

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)
  testthat::expect_snapshot(suppressWarnings(triangulate_bin_centroids(hex_bin_df = df_bin_mean,
                                                             x = "c_x", y = "c_y")))

})

test_that("generate_edge_info() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  suppressWarnings(tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y"))
  testthat::expect_snapshot(generate_edge_info(triangular_object = tr1_object))

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

  suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
  testthat::expect_snapshot(generate_edge_info(triangular_object = tr1_object_n))
})

test_that("cal_2d_dist() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  suppressWarnings(tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y"))
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  testthat::expect_snapshot(cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
                              start_y = "y_from", end_x = "x_to", end_y = "y_to",
                              select_col_vec = c("from", "to", "distance")))

  umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)

  suppressWarnings(tr1_object_n <- triangulate_bin_centroids(hex_bin_df = df_bin_mean, x = "c_x", y = "c_y"))
  tr_from_to_df_n <- generate_edge_info(triangular_object = tr1_object_n)

  testthat::expect_snapshot(cal_2d_dist(tr_from_to_df_coord = tr_from_to_df_n, start_x = "x_from",
                                        start_y = "y_from", end_x = "x_to", end_y = "y_to",
                                        select_col_vec = c("from", "to", "distance")))

})

test_that("colour_long_edges() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  suppressWarnings(tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids,
                                                           x = "c_x", y = "c_y"))
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_col_vec = c("from", "to", "distance"))

  vdiffr::expect_doppelganger("color-long-edges-basic with bin centroids",
                              colour_long_edges(distance_edges = distance_df,
                                                benchmark_value = 0.75,
                                                tr_from_to_df_coord = tr_from_to_df,
                                                distance_col = "distance"))

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

})

test_that("remove_long_edges() works", {

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

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  suppressWarnings(tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids,
                                                           x = "c_x", y = "c_y"))
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_col_vec = c("from", "to", "distance"))


  vdiffr::expect_doppelganger("remove_long_edges basic with bin centroids",
                              remove_long_edges(distance_edges = distance_df,
                                                benchmark_value = 0.75,
                                                tr_from_to_df_coord = tr_from_to_df,
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

})
