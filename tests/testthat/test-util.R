test_that("calculate_effective_x_bins() works", {

  testthat::expect_equal(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                    cell_area = 1), 6)

  testthat::expect_snapshot(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                       cell_area = 0), error = TRUE)
  testthat::expect_snapshot(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                       cell_area = Inf), error = TRUE)
  testthat::expect_snapshot(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                       cell_area = -3), error = TRUE)

  s_curve_noise_umap <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = NA, UMAP2 = 2.5, ID = 76))
  testthat::expect_snapshot(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                       cell_area = 1), error = TRUE)

  s_curve_noise_umap <- s_curve_noise_umap |> dplyr::filter(dplyr::row_number() != NROW(s_curve_noise_umap)) |>
    dplyr::bind_rows(data.frame(UMAP1 = Inf, UMAP2 = 2.5, ID = 76))
  testthat::expect_snapshot(calculate_effective_x_bins(.data = s_curve_noise_umap, x = UMAP1,
                                                       cell_area = 1), error = TRUE)


})


test_that("calculate_effective_shape_value() works", {

  testthat::expect_equal(calculate_effective_shape_value(.data = s_curve_noise_umap,
                                                         x = UMAP1, y = UMAP2), 2.0194144)

  s_curve_noise_umap_na <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = NA,
                                                                          UMAP2 = 2.5, ID = 76))
  #expect_error(calculate_effective_shape_value(data, x, y))
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_na,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

  s_curve_noise_umap_na <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = NA,
                                                                             UMAP2 = 2.5, ID = 76))
  #expect_error(calculate_effective_shape_value(data, x, y))
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_na,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

  s_curve_noise_umap_na <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = NA,
                                                                             UMAP2 = NA, ID = 76))
  #expect_error(calculate_effective_shape_value(data, x, y))
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_na,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

  s_curve_noise_umap_inf <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = Inf,
                                                                              UMAP2 = 2.5, ID = 76))
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_inf,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

  s_curve_noise_umap_inf <- s_curve_noise_umap |> dplyr::bind_rows(data.frame(UMAP1 = 1.4,
                                                                              UMAP2 = Inf, ID = 76))
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_inf,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

  s_curve_noise_umap_1 <- s_curve_noise_umap |> dplyr::filter(dplyr::row_number() == 1)
  testthat::expect_snapshot(calculate_effective_shape_value(.data = s_curve_noise_umap_1,
                                                            x = UMAP1, y = UMAP2), error = TRUE)

})


