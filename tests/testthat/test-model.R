set.seed(20240110)

test_that("fit_highd_model() works", {

  testthat::expect_snapshot(fit_highd_model(highd_data = scurve,
                                            nldr_data = scurve_umap,
                                            b1 = 4, q = 0.1,
                                            hd_thresh = 5))

  testthat::expect_error(fit_highd_model(highd_data = scurve,
                                         nldr_data = scurve_umap,
                                         b1 = 1, q = 0.1,
                                         hd_thresh = 5))

  testthat::expect_length(fit_highd_model(highd_data = scurve,
                                          nldr_data = scurve_umap,
                                          b1 = 4, q = 0.1,
                                          hd_thresh = 5), 5)

  testthat::expect_error(fit_highd_model(highd_data = scurve,
                                         nldr_data = scurve_umap,
                                         b1 = 4, q = 0.01,
                                         hd_thresh = 5))

  testthat::expect_error(fit_highd_model(highd_data = scurve,
                                         nldr_data = scurve_umap,
                                         b1 = 4, q = 0.3,
                                         hd_thresh = 5))

  testthat::expect_snapshot(fit_highd_model(highd_data = scurve,
                                            nldr_data = scurve_umap,
                                            b1 = 15, q = 0.1,
                                            hd_thresh = 5))


})

