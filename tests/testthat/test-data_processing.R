test_that("calc_y_max() works", {
  testthat::expect_equal(calc_y_max(aspect_ratio = 2.019414,
                                    hex_ratio = 0.2309401), 2.0784609)
  testthat::expect_error(calc_y_max(aspect_ratio = NA,
                                    hex_ratio = 0.2309401))

  testthat::expect_error(calc_y_max(aspect_ratio = 2.019414,
                                    hex_ratio = NA))
  testthat::expect_error(calc_y_max(aspect_ratio = Inf,
                                    hex_ratio = 0.2309401))
  testthat::expect_error(calc_y_max(aspect_ratio = 2.019414,
                                    hex_ratio = Inf))
  testthat::expect_error(calc_y_max(aspect_ratio = -2.019414,
                                    hex_ratio = 0.2309401))
  testthat::expect_error(calc_y_max(aspect_ratio = 2.019414,
                                    hex_ratio = -0.2309401))
})


test_that("gen_scaled_data() works", {
  testthat::expect_snapshot(gen_scaled_data(data = s_curve_noise_umap,
                                            x = "UMAP1", y = "UMAP2",
                                            hex_ratio = NA))
  testthat::expect_length(gen_scaled_data(data = s_curve_noise_umap,
                                            x = "UMAP1", y = "UMAP2",
                                            hex_ratio = NA), 2)
})
