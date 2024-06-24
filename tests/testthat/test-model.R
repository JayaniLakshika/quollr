test_that("fit_highd_model() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))

  testthat::expect_snapshot(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = TRUE,
                                            is_rm_lwd_hex = FALSE,
                                            col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = TRUE,
                                         is_rm_lwd_hex = FALSE,
                                         benchmark_to_rm_lwd_hex = 0.25,
                                         col_start_highd = "x"))

  testthat::expect_length(fit_highd_model(training_data = s_curve_noise_training,
                                          emb_df = s_curve_noise_umap_scaled,
                                          bin1 = 3, r2 = r2,
                                          is_bin_centroid = TRUE,
                                          is_rm_lwd_hex = FALSE,
                                          col_start_highd = "x"), 2)


  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = TRUE,
                                            is_rm_lwd_hex = TRUE,
                                            benchmark_to_rm_lwd_hex = 0.1,
                                            col_start_highd = "x"))

  testthat::expect_snapshot(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = TRUE,
                                            is_rm_lwd_hex = TRUE,
                                            benchmark_to_rm_lwd_hex = 0.4,
                                            col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = TRUE,
                                         is_rm_lwd_hex = TRUE,
                                         benchmark_to_rm_lwd_hex = 1.5,
                                         col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = TRUE,
                                         is_rm_lwd_hex = TRUE,
                                         benchmark_to_rm_lwd_hex = 0,
                                         col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = TRUE,
                                         is_rm_lwd_hex = FALSE,
                                         benchmark_to_rm_lwd_hex = 0.4,
                                         col_start_highd = "x"))

  testthat::expect_snapshot(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = FALSE,
                                            is_rm_lwd_hex = FALSE,
                                            col_start_highd = "x"))

  testthat::expect_length(fit_highd_model(training_data = s_curve_noise_training,
                                          emb_df = s_curve_noise_umap_scaled,
                                          bin1 = 3, r2 = r2,
                                          is_bin_centroid = FALSE,
                                          is_rm_lwd_hex = FALSE,
                                          col_start_highd = "x"), 2)

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = FALSE,
                                            is_rm_lwd_hex = TRUE,
                                            benchmark_to_rm_lwd_hex = 0.1,
                                            col_start_highd = "x"))

  testthat::expect_snapshot(fit_highd_model(training_data = s_curve_noise_training,
                                            emb_df = s_curve_noise_umap_scaled,
                                            bin1 = 3, r2 = r2,
                                            is_bin_centroid = FALSE,
                                            is_rm_lwd_hex = TRUE,
                                            benchmark_to_rm_lwd_hex = 0.4,
                                            col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = FALSE,
                                         is_rm_lwd_hex = TRUE,
                                         benchmark_to_rm_lwd_hex = 1.5,
                                         col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = FALSE,
                                         is_rm_lwd_hex = TRUE,
                                         benchmark_to_rm_lwd_hex = 0,
                                         col_start_highd = "x"))

  testthat::expect_error(fit_highd_model(training_data = s_curve_noise_training,
                                         emb_df = s_curve_noise_umap_scaled,
                                         bin1 = 3, r2 = r2,
                                         is_bin_centroid = FALSE,
                                         is_rm_lwd_hex = FALSE,
                                         benchmark_to_rm_lwd_hex = 0.4,
                                         col_start_highd = "x")())




})

