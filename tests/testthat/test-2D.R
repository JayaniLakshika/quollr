test_that("gen_centroids() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  all_centroids_df <- gen_centroids(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                    r2 = range_umap2)

  testthat::expect_snapshot(all_centroids_df)

})

test_that("gen_hex_coord() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  num_bins_list <- calc_bins_y(bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = range_umap2)
  width <- num_bins_list$a1

  all_centroids_df <- gen_centroids(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                    r2 = range_umap2)

  all_hex_coord <- gen_hex_coord(centroids_df = all_centroids_df, a1 = width)
  testthat::expect_snapshot(all_hex_coord)

})

test_that("assign_data() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  all_centroids_df <- gen_centroids(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                    r2 = range_umap2)

  umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
  centroid_df = all_centroids_df)
  testthat::expect_snapshot(umap_with_hb_id)

})

test_that("compute_std_counts() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  all_centroids_df <- gen_centroids(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                    r2 = range_umap2)

  umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
                                 centroid_df = all_centroids_df)
  std_count_df <- compute_std_counts(data = umap_with_hb_id)
  testthat::expect_snapshot(std_count_df)

})

test_that("find_pts() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  all_centroids_df <- gen_centroids(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                    r2 = range_umap2)

  umap_with_hb_id <- assign_data(data = s_curve_noise_umap_scaled,
                                 centroid_df = all_centroids_df)

  testthat::expect_snapshot(find_pts(data_hb = umap_with_hb_id))

})

test_that("hex_binning() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))

  testthat::expect_snapshot(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                                  bin1 = 3, s1 = -0.1,
                                                  s2 = -0.1, r2 = range_umap2))

})

test_that("find_non_empty_bins() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  testthat::expect_snapshot(find_non_empty_bins(data = s_curve_noise_umap_scaled,
                                                non_empty_bins = 6,
                                                r2 = range_umap2))

  testthat::expect_error(find_non_empty_bins(data = s_curve_noise_umap_scaled,
                                             non_empty_bins = 10,
                                             r2 = range_umap2))

})

test_that("extract_hexbin_centroids() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)
  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts

  testthat::expect_snapshot(extract_hexbin_centroids(centroids_df = all_centroids_df,
                                                     counts_df = counts_df))

})

test_that("extract_hexbin_mean() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  umap_with_hb_id <- hb_obj$data_hb_id
  counts_df <- hb_obj$std_cts

  testthat::expect_snapshot(extract_hexbin_mean(data_hb = umap_with_hb_id,
                                                counts_df = counts_df))

})

test_that("tri_bin_centroids() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  testthat::expect_snapshot(suppressWarnings(tri_bin_centroids(hex_df = df_bin_centroids,
                                                               x = "c_x", y = "c_y")))

})

test_that("gen_edges() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  all_centroids_df <- hb_obj$centroid
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  suppressWarnings(tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids,
                                                   x = "c_x", y = "c_y"))
  testthat::expect_snapshot(gen_edges(tri_object = tr1_object))
})

test_that("cal_2d_dist() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
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

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
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

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, s1 = -0.1,
                        s2 = -0.1, r2 = range_umap2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
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
