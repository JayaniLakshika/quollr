test_that("geom_hexgrid() works", {

  ## To draw only for selected hexagons
  df_bin_centroids <- scurve_model_obj$model_2d |> dplyr::filter(n_h > 10)
  vdiffr::expect_doppelganger("geom_hexgrid basic with selected bin centroids", ggplot2::ggplot() +
                                geom_hexgrid(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y)))

  ## To draw the full hexagon grid
  df_bin_centroids_all <- scurve_model_obj$hb_obj$centroids

  vdiffr::expect_doppelganger("geom_hexgrid basic with all bin centroids", ggplot2::ggplot() +
                                geom_hexgrid(data = df_bin_centroids_all, mapping = ggplot2::aes(x = c_x, y = c_y)))
})
