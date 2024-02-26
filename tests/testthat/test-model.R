test_that("fit_high_d_model() works", {

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_length(fit_high_d_model(training_data = s_curve_noise_training,
                                           nldr_df_with_id = s_curve_noise_umap_scaled,
                                           x = "UMAP1", y = "UMAP2",
                                           num_bins_x = NA, num_bins_y = NA,
                                           x_start = NA, y_start = NA,
                                           buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                           is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
                                           benchmark_to_rm_lwd_hex = NA,
                                           is_avg_high_d = TRUE, column_start_text = "x"), 2)

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = 5, num_bins_y = 8,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = 0.4,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
                                          benchmark_to_rm_lwd_hex = 1.5,
                                          is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
                                          benchmark_to_rm_lwd_hex = 0,
                                          is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_error(fit_high_d_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
                                          benchmark_to_rm_lwd_hex = 0.4,
                                          is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = TRUE, column_start_text = "x"))

  testthat::expect_snapshot(fit_high_d_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             is_avg_high_d = FALSE, column_start_text = "x"))


})
