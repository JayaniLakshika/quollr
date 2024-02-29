test_that("gen_centroids() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(centroid_list <- gen_centroids(data = s_curve_noise_umap_scaled,
                                                  x = "UMAP1", y = "UMAP2",
                                                  num_bins_x = num_bins_x,
                                                  num_bins_y = num_bins_y,
                                                  x_start = NA, y_start = NA,
                                                  buffer_x = NA, buffer_y = NA,
                                                  hex_size = NA))
  testthat::expect_snapshot(centroid_list)

})

test_that("gen_hex_coord() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(centroid_list <- gen_centroids(data = s_curve_noise_umap_scaled,
                                                  x = "UMAP1", y = "UMAP2",
                                                  num_bins_x = num_bins_x,
                                                  num_bins_y = num_bins_y,
                                                  x_start = NA, y_start = NA,
                                                  buffer_x = NA, buffer_y = NA,
                                                  hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  suppressMessages(all_hex_coordinates_list <- gen_hex_coord(
    centroids_df = all_centroids_df, hex_size = NA))
  testthat::expect_snapshot(all_hex_coordinates_list)

})

test_that("assign_data() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(centroid_list <- gen_centroids(data = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(data = s_curve_noise_umap_scaled_rm_id,
  centroid_df = all_centroids_df, col_start = "UMAP")
  testthat::expect_snapshot(nldr_with_hb_id_list)

})

test_that("compute_std_counts() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(centroid_list <- gen_centroids(data = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(data = s_curve_noise_umap_scaled_rm_id,
  centroid_df = all_centroids_df, col_start = "UMAP")
  umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
  std_counst_list <- compute_std_counts(data_hex_id = umap_with_hb_id)
  testthat::expect_snapshot(std_counst_list)

})

test_that("find_pts() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(centroid_list <- gen_centroids(data = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA))
  all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
  s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
  nldr_with_hb_id_list <- assign_data(data = s_curve_noise_umap_scaled_rm_id,
  centroid_df = all_centroids_df, col_start = "UMAP")
  umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
  umap_with_hb_id <- umap_with_hb_id |> dplyr::mutate(ID = s_curve_noise_umap_scaled$ID)

  testthat::expect_snapshot(find_pts(data_hex_id = umap_with_hb_id))

})

test_that("hex_binning() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y

  testthat::expect_snapshot(suppressMessages(hex_binning(data = s_curve_noise_umap_scaled,
                                                         x = "UMAP1", y = "UMAP2",
                                                         num_bins_x = num_bins_x,
                                                         num_bins_y = num_bins_y,
                                                         x_start = NA, y_start = NA,
                                                         buffer_x = NA, buffer_y = NA,
                                                         hex_size = NA, col_start = "UMAP")))

})

test_that("find_non_empty_bins() works", {

  testthat::expect_snapshot(suppressMessages(find_non_empty_bins(data = s_curve_noise_umap_scaled,
                                                                 x = "UMAP1",
                                                                 y = "UMAP2",
                                                                 non_empty_bins = 10,
                                                                 x_start = NA,
                                                                 y_start = NA,
                                                                 buffer_x = NA,
                                                                 buffer_y = NA,
                                                                 hex_size = NA,
                                                                 col_start = "UMAP")
  ))

  testthat::expect_error(find_non_empty_bins(data = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             non_empty_bins = NA, x_start = NA,
                                             y_start = NA, buffer_x = NA,
                                             buffer_y = NA, hex_size = NA,
                                             col_start = "UMAP")
  )

})

test_that("extract_hexbin_centroids() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
  x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
  num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
  buffer_y = NA, hex_size = NA, col_start = "UMAP"))
  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))

  testthat::expect_snapshot(extract_hexbin_centroids(centroids_df = all_centroids_df,
                                                     counts_df = counts_df))

})

test_that("tri_bin_centroids() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = NA,
                                         y_start = NA, buffer_x = NA, buffer_y = NA,
                                         hex_size = NA, col_start = "UMAP"))
  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  testthat::expect_snapshot(suppressWarnings(tri_bin_centroids(hex_df = df_bin_centroids,
                                                               x = "c_x", y = "c_y")))

})

test_that("gen_edges() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
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
  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  testthat::expect_snapshot(gen_edges(tri_object = tr1_object))
})

test_that("cal_2d_dist() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
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
  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  testthat::expect_snapshot(cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                                        start_y = "y_from", end_x = "x_to",
                                        end_y = "y_to",
                                        select_vars = c("from", "to", "distance")))



})

test_that("vis_lg_mesh() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
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
  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to",
                             end_y = "y_to",
                             select_vars = c("from", "to", "distance"))

  vdiffr::expect_doppelganger("Triangular mesh with coloured long edges",
                              vis_lg_mesh(distance_edges = distance_df,
                                          benchmark_value = 0.75,
                                          tr_coord_df = tr_from_to_df,
                                          distance_col = "distance"))

})

test_that("vis_rmlg_mesh() works", {

  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled,
                                              x = "UMAP1", y = "UMAP2",
                                              hex_size = NA, buffer_x = NA,
                                              buffer_y = NA))
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
  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  tr_from_to_df <- gen_edges(tri_object = tr1_object)

  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to",
                             end_y = "y_to",
                             select_vars = c("from", "to", "distance"))

  vdiffr::expect_doppelganger("Triangular mesh after removing long edges",
                              vis_rmlg_mesh(distance_edges = distance_df,
                                            benchmark_value = 0.75,
                                            tr_coord_df = tr_from_to_df,
                                            distance_col = "distance"))

})
