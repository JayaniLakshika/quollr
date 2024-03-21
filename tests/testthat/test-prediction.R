test_that("predict_emb() works", {

  suppressMessages(model <- fit_highd_model(training_data = s_curve_noise_training,
                                            x = "UMAP1", y = "UMAP2",
                                            nldr_df_with_id = s_curve_noise_umap_scaled,
                                            col_start_2d = "UMAP", col_start_highd = "x"))
  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin

  testthat::expect_snapshot(predict_emb(test_data = s_curve_noise_training,
                                        df_bin_centroids = df_bin_centroids,
                                        df_bin = df_bin, type_NLDR = "UMAP"))

  testthat::expect_snapshot(predict_emb(test_data = s_curve_noise_test,
                                        df_bin_centroids = df_bin_centroids,
                                        df_bin = df_bin, type_NLDR = "UMAP"))

})

test_that("compute_aic() works", {

  p <- 5
  mse <- 1500
  num_bins <- 10
  num_obs <- 100

  testthat::expect_equal(compute_aic(p, mse, num_bins, num_obs), 3756.6102)

  testthat::expect_error(compute_aic(p = Inf, mse, num_bins, num_obs))

  testthat::expect_error(compute_aic(p = 0, mse, num_bins, num_obs))

  testthat::expect_error(compute_aic(p = NA, mse, num_bins, num_obs))

  testthat::expect_error(compute_aic(p, mse, num_bins = NA, num_obs))

  testthat::expect_error(compute_aic(p, mse, num_bins, num_obs = NA))

  testthat::expect_error(compute_aic(p, mse = NA, num_bins, num_obs))
})

test_that("gen_summary() works", {

  suppressMessages(model <- fit_highd_model(training_data = s_curve_noise_training,
                                            x = "UMAP1", y = "UMAP2",
                                            nldr_df_with_id = s_curve_noise_umap_scaled,
                                            col_start_2d = "UMAP", col_start_highd = "x"))
  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin
  pred_emb_list <- predict_emb(test_data = s_curve_noise_training,
                               df_bin_centroids = df_bin_centroids,
                               df_bin = df_bin, type_NLDR = "UMAP")
  pred_df_test <- as.data.frame(do.call(cbind, pred_emb_list))


  testthat::expect_snapshot(gen_summary(test_data = s_curve_noise_training,
                                        prediction_df = pred_df_test,
                                        df_bin = df_bin, col_start = "x"))


  pred_emb_list_n <- predict_emb(test_data = s_curve_noise_test,
                                 df_bin_centroids = df_bin_centroids,
                                 df_bin = df_bin, type_NLDR = "UMAP")
  pred_df_test_n <- as.data.frame(do.call(cbind, pred_emb_list_n))

  testthat::expect_snapshot(gen_summary(test_data = s_curve_noise_test,
                                        prediction_df = pred_df_test_n,
                                        df_bin = df_bin, col_start = "x"))

})
