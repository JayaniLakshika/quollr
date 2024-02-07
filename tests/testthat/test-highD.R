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
