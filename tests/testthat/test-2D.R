test_that("extract_hexbin_centroids() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  result <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value, x = "UMAP1", y = "UMAP2")
  hexdf_data <- result$hexdf_data
  hb_data <- result$hb_data
  expect_snapshot(hexdf_data)
  expect_snapshot(hb_data)

})

test_that("extract_hexbin_mean() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  result <- extract_hexbin_mean(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value, x = "UMAP1", y = "UMAP2")
  hexdf_data <- result$hexdf_data
  hb_data <- result$hb_data
  expect_snapshot(hexdf_data)
  expect_snapshot(hb_data)

})

test_that("triangulate_bin_centroids() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  expect_snapshot(suppressWarnings(triangulate_bin_centroids(df_bin_centroids, x, y)))

})

test_that("generate_edge_info() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  suppressWarnings(tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y))

  expect_snapshot(generate_edge_info(triangular_object = tr1_object))
})

test_that("cal_2d_dist() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  suppressWarnings(tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y))
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  expect_snapshot(cal_2d_dist(tr_from_to_df, start_x = "x_from", start_y = "y_from",
                              end_x = "x_to", end_y = "y_to", select_col_vec = c("from", "to", "distance")))
})

test_that("colour_long_edges() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
  distance_df <- cal_2d_dist(tr_from_to_df)

  vdiffr::expect_doppelganger("color_long_edges basic", colour_long_edges(.data = distance_df,
                                                                          benchmark_value = 5.4,
                                                                          triangular_object = tr1_object,
                                                                          distance_col = "distance"))

})

test_that("remove_long_edges() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
                                                 num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
  distance_df <- cal_2d_dist(tr_from_to_df)

  vdiffr::expect_doppelganger("remove_long_edges basic", remove_long_edges(.data = distance_df,
                                                                           benchmark_value = 5.4,
                                                                           triangular_object = tr1_object,
                                                                           distance_col = "distance"))

})

test_that("generate_full_grid_centroids() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data

  expect_snapshot(generate_full_grid_centroids(df_bin_centroids))

})

test_that("full_hex_grid() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  expect_snapshot(full_hex_grid(full_centroid_df))

})

test_that("map_hexbin_id() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  expect_snapshot(map_hexbin_id(full_centroid_df, df_bin_centroids))

})

test_that("map_polygon_id() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
  hex_grid <- full_hex_grid(full_centroid_df)
  full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)

  expect_snapshot(map_polygon_id(full_grid_with_hexbin_id, hex_grid))

})

test_that("generate_full_grid_info() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data

  expect_snapshot(generate_full_grid_info(df_bin_centroids))

})

test_that("find_pts_in_hexbins() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
  hex_grid <- full_hex_grid(full_centroid_df)
  full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
  UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)

  expect_snapshot(find_pts_in_hexbins(full_grid_with_hexbin_id, nldr_data_with_hb_id = UMAP_data_with_hb_id))

})

test_that("find_pts_in_hexbins() works", {

  shape_value <- 1.833091
  non_empty_bins <- 3

  expect_snapshot(find_non_empty_bins(nldr_df = s_curve_noise_umap, x = "UMAP1", y = "UMAP2",
                                      shape_val = shape_value, non_empty_bins))

  expect_error(find_non_empty_bins(nldr_df = s_curve_noise_umap, x = "UMAP1", y = "UMAP2",
                                      shape_val = shape_value, non_empty_bins = 20))

})
