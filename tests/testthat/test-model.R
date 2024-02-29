test_that("fit_highd_model() works", {

  testthat::expect_snapshot(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             col_start_2d = "UMAP", col_start_highd = "x")))

  testthat::expect_length(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                           nldr_df_with_id = s_curve_noise_umap_scaled,
                                           x = "UMAP1", y = "UMAP2",
                                           num_bins_x = NA, num_bins_y = NA,
                                           x_start = NA, y_start = NA,
                                           buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                           is_rm_lwd_hex = FALSE,
                                           benchmark_to_rm_lwd_hex = NA,
                                           col_start_2d = "UMAP",
                                           col_start_highd = "x")), 2)

  testthat::expect_snapshot(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = 5, num_bins_y = 8,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_rm_lwd_hex = FALSE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             col_start_2d = "UMAP",
                                             col_start_highd = "x")))

  testthat::expect_snapshot(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = NA,
                                             col_start_2d = "UMAP",
                                             col_start_highd = "x")))

  testthat::expect_snapshot(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                             nldr_df_with_id = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = NA, num_bins_y = NA,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                             is_rm_lwd_hex = TRUE,
                                             benchmark_to_rm_lwd_hex = 0.4,
                                             col_start_2d = "UMAP",
                                             col_start_highd = "x")))

  testthat::expect_error(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_rm_lwd_hex = TRUE,
                                          benchmark_to_rm_lwd_hex = 1.5,
                                          col_start_2d = "UMAP",
                                          col_start_highd = "x")))

  testthat::expect_error(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_rm_lwd_hex = TRUE,
                                          benchmark_to_rm_lwd_hex = 0,
                                          col_start_2d = "UMAP",
                                          col_start_highd = "x")))

  testthat::expect_error(suppressMessages(fit_highd_model(training_data = s_curve_noise_training,
                                          nldr_df_with_id = s_curve_noise_umap_scaled,
                                          x = "UMAP1", y = "UMAP2",
                                          num_bins_x = NA, num_bins_y = NA,
                                          x_start = NA, y_start = NA,
                                          buffer_x = NA, buffer_y = NA,  hex_size = NA,
                                          is_rm_lwd_hex = FALSE,
                                          benchmark_to_rm_lwd_hex = 0.4,
                                          col_start_2d = "UMAP",
                                          col_start_highd = "x")))



})
