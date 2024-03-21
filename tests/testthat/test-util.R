test_that("calc_bins() works", {

  testthat::expect_length(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                     x = "UMAP1", y = "UMAP2",
                                                     hex_size = NA, buffer_x = NA,
                                                     buffer_y = NA)), 2)

  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = -0.1, buffer_x = NA,
                                                    buffer_y = NA)))
  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = 0, buffer_x = NA,
                                                    buffer_y = NA))
  )
  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = Inf, buffer_x = NA,
                                                    buffer_y = NA))
  )
  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = -Inf, buffer_x = NA,
                                                    buffer_y = NA))
  )

  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = 0.5,
                                                    buffer_y = NA))
  )
  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = -0.3,
                                                    buffer_y = NA))
  )

  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = 0.5))
  )
  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = -0.3))
  )

  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = -0.4,
                                                    buffer_y = -0.3))
  )

  testthat::expect_error(suppressMessages(calc_bins(data = s_curve_noise_umap_scaled,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = 0.5,
                                                    buffer_y = 0.8))
  )

  umap_df1 <- s_curve_noise_umap_scaled |> dplyr::bind_rows(data.frame(UMAP1 = NA,
                                                                      UMAP2 = 2.5, ID = 76))
  testthat::expect_error(suppressMessages(calc_bins(data = umap_df1,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = NA)))

  umap_df2 <- s_curve_noise_umap_scaled |> dplyr::bind_rows(data.frame(UMAP1 = 2.5,
                                                                       UMAP2 = NA, ID = 76))
  testthat::expect_error(suppressMessages(calc_bins(data = umap_df2,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = NA)))

  umap_df3 <- s_curve_noise_umap_scaled |>
    dplyr::filter(dplyr::row_number() != NROW(s_curve_noise_umap_scaled)) |>
    dplyr::bind_rows(data.frame(UMAP1 = Inf, UMAP2 = 2.5, ID = 76))
  testthat::expect_error(suppressMessages(calc_bins(data = umap_df3,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = NA)))

  umap_df4 <- s_curve_noise_umap_scaled |>
    dplyr::filter(dplyr::row_number() != NROW(s_curve_noise_umap_scaled)) |>
    dplyr::bind_rows(data.frame(UMAP1 = 2.5, UMAP2 = Inf, ID = 76))
  testthat::expect_error(suppressMessages(calc_bins(data = umap_df4,
                                                    x = "UMAP1", y = "UMAP2",
                                                    hex_size = NA, buffer_x = NA,
                                                    buffer_y = NA)))


})
