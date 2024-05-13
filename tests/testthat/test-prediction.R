test_that("predict_emb() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = r2,
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

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = r2,
                           col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin


  testthat::expect_snapshot(glance(df_bin_centroids = df_bin_centroids,
                                   df_bin = df_bin,
                                   training_data = s_curve_noise_training,
                                   newdata = NULL, type_NLDR = "UMAP",
                                   col_start = "x"))

  testthat::expect_snapshot(glance(df_bin_centroids = df_bin_centroids,
                                   df_bin = df_bin,
                                   training_data = s_curve_noise_training,
                                   newdata = s_curve_noise_test,
                                   type_NLDR = "UMAP", col_start = "x"))

})


test_that("augment() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  model <- fit_highd_model(training_data = s_curve_noise_training,
                           emb_df = s_curve_noise_umap_scaled,
                           r2 = r2,
                           col_start_highd = "x")

  df_bin_centroids <- model$df_bin_centroids
  df_bin <- model$df_bin
  testthat::expect_snapshot(augment(df_bin_centroids = df_bin_centroids,
                                    df_bin = df_bin,
                                    training_data = s_curve_noise_training,
                                    newdata = NULL, type_NLDR = "UMAP",
                                    col_start = "x"))

  testthat::expect_snapshot(augment(df_bin_centroids = df_bin_centroids,
                                    df_bin = df_bin,
                                    training_data = s_curve_noise_training,
                                    newdata = s_curve_noise_test,
                                    type_NLDR = "UMAP", col_start = "x"))

})

