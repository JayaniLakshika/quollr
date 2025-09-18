set.seed(20240110)

test_that("gen_axes() works", {

  projection_df <- cbind(
  c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
  c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

  testthat::expect_snapshot(gen_axes(proj = projection_df,
                                     axis_labels = paste0("x", 1:7)))
})

test_that("gen_axes() works", {

  projection_df <- cbind(
  c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
  c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

  df_bin <- scurve_model_obj$model_highd
  edge_data <- scurve_model_obj$trimesh_data

  testthat::expect_snapshot(get_projection(projection = projection_df, proj_scale = 1,
                                           highd_data = scurve, model_highd = df_bin,
                                           trimesh_data = edge_data,
                                           axis_param = list(limits = 1,
                                                             axis_scaled = 3,
                                                             axis_pos_x = -0.72,
                                                             axis_pos_y = -0.72,
                                                             threshold = 0.09)))
})

test_that("gen_proj_langevitour() works", {

  projection_df <- cbind(
  c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
  c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

  df_bin <- scurve_model_obj$model_highd
  edge_data <- scurve_model_obj$trimesh_data

  proj_obj1 <- get_projection(projection = projection_df, proj_scale = 1,
  highd_data = scurve, model_highd = df_bin,
  trimesh_data = edge_data,
  axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
  axis_pos_y = -0.72, threshold = 0.09))

  vdiffr::expect_doppelganger("2-D projection from langevitour",
                              plot_proj(proj_obj = proj_obj1,
                                        plot_limits = c(-1, 1)))


})
