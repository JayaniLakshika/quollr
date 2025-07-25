test_that("compute_mean_density_hex() works", {

  testthat::expect_snapshot(compute_mean_density_hex(model_2d = scurve_model_obj$model_2d,
                                                     b1 = 4))

})


test_that("find_low_dens_hex() works", {

  testthat::expect_snapshot(find_low_dens_hex(model_2d = scurve_model_obj$model_2d,
                                              b1 = 4,
                                              benchmark_mean_dens = 0.05))

  testthat::expect_snapshot(find_low_dens_hex(model_2d = scurve_model_obj$model_2d,
                                              b1 = 4,
                                              benchmark_mean_dens = 0.1))

  testthat::expect_error(find_low_dens_hex(model_2d = scurve_model_obj$model_2d,
                                           b1 = NA,
                                           benchmark_mean_dens = 0.05))

})
