test_that("fit_high_d_model() works", {

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = NA, shape_val = NA,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_length(fit_high_d_model(training_data = s_curve_noise_training,
                                           nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                           y = "UMAP1", cell_area = 1, num_bins_x = NA, shape_val = NA,
                                           is_bin_centroid = TRUE,
                                           is_rm_lwd_hex = FALSE,
                                           benchmark_to_rm_lwd_hex = NA,
                                           is_avg_high_d = TRUE, column_start_text = "x"), 2)

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = 0.2,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = 1.5,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = 0,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = TRUE,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = 0.2,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = FALSE,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap, x = "UMAP1",
                                             y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
                                             is_bin_centroid = FALSE,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = FALSE, column_start_text = "x"))


})
