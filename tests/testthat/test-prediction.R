test_that("compute_aic() works", {

  p <- 5
  total <- 1500
  num_bins <- 10
  num_obs <- 100

  testthat::expect_equal(compute_aic(p, total, num_bins, num_obs), 2951.8912)

  testthat::expect_snapshot(compute_aic(p = Inf, total, num_bins, num_obs), error = TRUE)

  testthat::expect_snapshot(compute_aic(p = 0, total, num_bins, num_obs), error = TRUE)

  testthat::expect_snapshot(compute_aic(p = NA, total, num_bins, num_obs), error = TRUE)

  testthat::expect_snapshot(compute_aic(p, total, num_bins = NA, num_obs), error = TRUE)

  testthat::expect_snapshot(compute_aic(p, total, num_bins, num_obs = NA), error = TRUE)

  testthat::expect_snapshot(compute_aic(p, total = c(), num_bins, num_obs), error = TRUE)

  testthat::expect_snapshot(compute_aic(p, total = c(1, 2, NA), num_bins, num_obs), error = TRUE)
})


test_that("predict_hex_id() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_mean(nldr_df = s_curve_noise_umap, num_bins_x,
                                            shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  testthat::expect_snapshot(predict_hex_id(df_bin_centroids = df_bin_centroids,
                                           nldr_df_test = s_curve_noise_umap,
                                           x = "UMAP1", y = "UMAP2"))

})

test_that("generate_eval_df() works", {

  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_mean(nldr_df = s_curve_noise_umap, num_bins_x,
  shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), UMAP_data_with_hb_id)
  df_bin <- avg_highD_data(df_all)
  pred_df_test <- predict_2d_embeddings(test_data = s_curve_noise_training,
  df_bin_centroids = df_bin_centroids, df_bin = df_bin, type_NLDR = "UMAP")
  testthat::expect_snapshot(generate_eval_df(data = s_curve_noise, prediction_df = pred_df_test,
                                             df_bin_centroids = df_bin_centroids,
                                             df_bin = df_bin,
                                             col_start = "x"))

})

test_that("predict_2d_embeddings() works", {

  model <- fit_high_d_model(training_data = s_curve_noise_training,
  nldr_df_with_id = s_curve_noise_umap)
  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin
  testthat::expect_snapshot(predict_2d_embeddings(test_data = s_curve_noise_test,
                                                  df_bin_centroids = df_bin_centroids,
                                                  df_bin = df_bin, type_NLDR = "UMAP"))

})
