set.seed(20240110)

test_that("predict_emb() works", {

  testthat::expect_snapshot(predict_emb(highd_data = scurve,
                                        model_highd = scurve_model_obj$model_highd,
                                        model_2d = scurve_model_obj$model_2d))

})

test_that("glance() works", {

  model_fit <- fit_highd_model(highd_data = scurve,
                               nldr_data = scurve_umap,
                               b1 = 4, q = 0.1,
                               hd_thresh = 5)

  testthat::expect_snapshot(glance(x = model_fit,
                                   highd_data = scurve))

})


test_that("augment() works", {

  model_fit <- fit_highd_model(highd_data = scurve,
                               nldr_data = scurve_umap,
                               b1 = 4, q = 0.1,
                               hd_thresh = 5)

  testthat::expect_snapshot(augment(x = model_fit,
                                    highd_data = scurve))

})

test_that("gen_diffbin1_errors() works", {

  testthat::expect_snapshot(gen_diffbin1_errors(highd_data = scurve,
                                                nldr_data = scurve_umap))

})

test_that("plot_hbe_layouts() works", {

  design <- gen_design(n_right = 6, ncol_right = 2)
  result <- plot_hbe_layouts(plots = scurve_plts,
                    design = design)

  testthat::expect_s3_class(result, "patchwork")

})

