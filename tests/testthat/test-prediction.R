test_that("predict_emb() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = range_umap2,
                           col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin

  testthat::expect_snapshot(predict_emb(test_data = s_curve_noise_training,
                                        df_bin_centroids = df_bin_centroids,
                                        df_bin = df_bin, type_NLDR = "UMAP"))

  testthat::expect_snapshot(predict_emb(test_data = s_curve_noise_test,
                                        df_bin_centroids = df_bin_centroids,
                                        df_bin = df_bin, type_NLDR = "UMAP"))

})

test_that("glance() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = range_umap2,
                           col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin
  pred_df_training <- predict_emb(test_data = s_curve_noise_training,
                               df_bin_centroids = df_bin_centroids,
                               df_bin = df_bin, type_NLDR = "UMAP")


  testthat::expect_snapshot(glance(test_data = s_curve_noise_training,
                                        prediction_df = pred_df_training,
                                        df_bin = df_bin, col_start = "x"))


  pred_df_test <- predict_emb(test_data = s_curve_noise_test,
                                 df_bin_centroids = df_bin_centroids,
                                 df_bin = df_bin, type_NLDR = "UMAP")
  testthat::expect_snapshot(glance(test_data = s_curve_noise_test,
                                        prediction_df = pred_df_test,
                                        df_bin = df_bin, col_start = "x"))

})


test_that("augment() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = range_umap2,
                           col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin
  testthat::expect_snapshot(augment(df_bin_centroids = df_bin_centroids,
                                    df_bin = df_bin, training_data = s_curve_noise_training,
                                    newdata = NULL, type_NLDR = "UMAP",
                                    col_start = "x"))

  testthat::expect_snapshot(augment(df_bin_centroids = df_bin_centroids,
                                    df_bin = df_bin, training_data = s_curve_noise_training,
                                    newdata = s_curve_noise_test,
                                    type_NLDR = "UMAP", col_start = "x"))

})

