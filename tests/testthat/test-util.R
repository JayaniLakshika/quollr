test_that("calculate_effective_x_bins() works", {

  testthat::expect_equal(suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = NA, buffer_x = NA)), 5)

  testthat::expect_error(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = -0.1, buffer_x = NA))
  testthat::expect_error(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = 0, buffer_x = NA))
  testthat::expect_error(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = Inf, buffer_x = NA))
  testthat::expect_error(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = -Inf, buffer_x = NA))

  testthat::expect_error(suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = NA, buffer_x = 0.5)))
  testthat::expect_error(suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", hex_size = NA, buffer_x = -0.3)))

  umap_df <- s_curve_noise_umap_scaled |> dplyr::bind_rows(data.frame(UMAP1 = NA,
                                                                      UMAP2 = 2.5, ID = 76))
  testthat::expect_error(calculate_effective_x_bins(nldr_df = umap_df, x = "UMAP1",
                                                    hex_size = NA, buffer_x = NA))

  umap_df <- s_curve_noise_umap_scaled |>
    dplyr::filter(dplyr::row_number() != NROW(s_curve_noise_umap_scaled)) |>
    dplyr::bind_rows(data.frame(UMAP1 = Inf, UMAP2 = 2.5, ID = 76))
  testthat::expect_error(calculate_effective_x_bins(nldr_df = umap_df, x = "UMAP1",
                                                    hex_size = NA, buffer_x = NA))


})


test_that("calculate_effective_y_bins() works", {

  testthat::expect_equal(suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = NA, buffer_y = NA)), 8)

  testthat::expect_error(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = -0.1, buffer_y = NA))
  testthat::expect_error(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = 0, buffer_y = NA))
  testthat::expect_error(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = Inf, buffer_y = NA))
  testthat::expect_error(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = -Inf, buffer_y = NA))

  testthat::expect_error(suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = NA, buffer_y = 0.5)))
  testthat::expect_error(suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                                    y = "UMAP2", hex_size = NA, buffer_y = -0.3)))

  umap_df <- s_curve_noise_umap_scaled |> dplyr::bind_rows(data.frame(UMAP1 = 2.5,
                                                                      UMAP2 = NA, ID = 76))
  testthat::expect_error(calculate_effective_y_bins(nldr_df = umap_df, y = "UMAP2",
                                                    hex_size = NA, buffer_y = NA))

  umap_df <- s_curve_noise_umap_scaled |>
    dplyr::filter(dplyr::row_number() != NROW(s_curve_noise_umap_scaled)) |>
    dplyr::bind_rows(data.frame(UMAP1 = 2.5, UMAP2 = Inf, ID = 76))
  testthat::expect_error(calculate_effective_y_bins(nldr_df = umap_df, y = "UMAP2",
                                                    hex_size = NA, buffer_y = NA))


})


