test_that("avg_highD_data() works", {

  training_data <- s_curve_noise_training
  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
  testthat::expect_snapshot(avg_highD_data(df_all))

})

test_that("compute_weights() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  hexdf_data <- hexbin_data_object$hexdf_data
  hb_obj <- hexbin_data_object$hb_data
  testthat::expect_snapshot(compute_weights(nldr_df = s_curve_noise_umap |> dplyr::select(-ID),
                                            hb_object = hb_obj))

})

test_that("weighted_highD_data() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  hexdf_data <- hexbin_data_object$hexdf_data
  hb_object <- hexbin_data_object$hb_data

  testthat::expect_snapshot(weighted_highD_data(training_data = s_curve_noise_training,
                                                nldr_df_with_id = s_curve_noise_umap,
                                                hb_object = hb_object, column_start_text = "x"))

})


test_that("show_langevitour() works", {

  training_data <- s_curve_noise_training
  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
  df_bin <- avg_highD_data(df_all)
  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
  distance_df <- cal_2d_dist(.data = tr_from_to_df)
  tour_widget <- show_langevitour(df_all, df_bin, df_bin_centroids, benchmark_value = 5.44,
  distance = distance_df, distance_col = "distance")

  # Test if the output is an HTML widget object
  #testthat::expect_type(tour_widget, "list")
  testthat::expect_s3_class(tour_widget, "langevitour")

})
