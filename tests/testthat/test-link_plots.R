set.seed(20240110)

test_that("comb_all_data_model() works", {

  testthat::expect_snapshot(comb_all_data_model(highd_data = scurve,
                                                nldr_data = scurve_umap,
                                                model_highd = scurve_model_obj$model_highd,
                                                model_2d = scurve_model_obj$model_2d))

})

test_that("show_link_plots() works", {

  if(interactive()) {

    df_exe <- comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap,
                                  model_highd = scurve_model_obj$model_highd,
                                  model_2d = scurve_model_obj$model_2d)
    edge_data <- scurve_model_obj$trimesh_data
    crosstalk_obj <- suppressWarnings(show_link_plots(point_data = df_exe,
                                                      edge_data = edge_data))

    testthat::expect_s3_class(crosstalk_obj, "shiny.tag")

  }

})


test_that("comb_all_data_model_error() works", {

  model_error <- augment(x = scurve_model_obj,
                         highd_data = scurve)

  testthat::expect_snapshot(comb_all_data_model_error(highd_data = scurve,
                                                      nldr_data = scurve_umap,
                                                      model_highd = scurve_model_obj$model_highd,
                                                      model_2d = scurve_model_obj$model_2d,
                                                      error_data = model_error))

})


test_that("show_link_plots() works", {

  if(interactive()) {

    model_error <- augment(x = scurve_model_obj, highd_data = scurve)
    df_exe <- comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
                                        model_highd = scurve_model_obj$model_highd,
                                        model_2d = scurve_model_obj$model_2d,
                                        error_data = model_error)
    edge_data <- scurve_model_obj$trimesh_data
    crosstalk_obj <- suppressWarnings(show_error_link_plots(point_data = df_exe,
                                                            edge_data = edge_data))

    testthat::expect_s3_class(crosstalk_obj, "shiny.tag")

  }

})
