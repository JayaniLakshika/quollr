test_that("calc_bins_y() works", {

  range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
  testthat::expect_length(calc_bins_y(bin1 = 3, s1 = -0.1, s2 = -0.1,
                                      r2 = range_umap2), 2)

  testthat::expect_error(calc_bins_y(bin1 = 1, s1 = -0.1, s2 = -0.1,
                                     r2 = range_umap2))

  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.3, s2 = -0.1,
                                     r2 = range_umap2))
  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.1, s2 = -0.3,
                                     r2 = range_umap2))
  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.4, s2 = -0.3,
                                     r2 = range_umap2))

  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.01, s2 = -0.1,
                                     r2 = range_umap2))
  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.1, s2 = -0.01,
                                     r2 = range_umap2))
  testthat::expect_error(calc_bins_y(bin1 = 3, s1 = -0.01, s2 = -0.01,
                                     r2 = range_umap2))

})
