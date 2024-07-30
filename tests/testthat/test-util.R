test_that("calc_bins_y() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  testthat::expect_length(calc_bins_y(bin1 = 4, r2 = r2), 2)

  testthat::expect_error(calc_bins_y(bin1 = 1, r2 = r2))
  testthat::expect_error(calc_bins_y(bin1 = 4, q = 0.3, r2 = r2))
  testthat::expect_error(calc_bins_y(bin1 = 4, q = 0.01, r2 = r2))

})
