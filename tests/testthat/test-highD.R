set.seed(20240110)

test_that("avg_highd_data() works", {

  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
  testthat::expect_snapshot(avg_highd_data(highd_data = scurve,
                                           scaled_nldr_hexid = umap_with_hb_id))

})

test_that("comb_data_model() works", {

  testthat::expect_snapshot(comb_data_model(highd_data = scurve,
                                            model_highd = scurve_model_obj$model_highd,
                                            model_2d = scurve_model_obj$model_2d))

})


test_that("show_langevitour() works", {

  if(interactive()) {

    df_exe <- comb_data_model(highd_data = scurve,
                              model_highd = scurve_model_obj$model_highd,
                              model_2d = scurve_model_obj$model_2d)

    edge_data <- scurve_model_obj$trimesh_data

    tour_widget <- show_langevitour(point_data = df_exe, edge_data = edge_data)

    # Test if the output is an HTML widget object
    #testthat::expect_type(tour_widget, "list")
    testthat::expect_s3_class(tour_widget, "langevitour")

  }

})
