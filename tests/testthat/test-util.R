test_that("calc_bins_y() works", {

  testthat::expect_length(calc_bins_y(nldr_obj = scurve_model_obj$nldr_obj, b1 = 4, q = 0.1), 3)

  testthat::expect_error(calc_bins_y(nldr_obj = scurve_model_obj$nldr_obj, b1 = 1, q = 0.1))
  testthat::expect_error(calc_bins_y(calc_bins_y(nldr_obj = scurve_model_obj$nldr_obj, b1 = 4, q = 0.3)))
  testthat::expect_error(calc_bins_y(calc_bins_y(nldr_obj = scurve_model_obj$nldr_obj, b1 = 4, q = 0.01)))

})
