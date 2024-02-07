test_that("geom_trimesh() works", {
  num_bins_x <- 4
  shape_value <- 1.833091
  hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
  num_bins = num_bins_x, shape_val = shape_value)
  df_bin_centroids <- hexbin_data_object$hexdf_data
  vdiffr::expect_doppelganger("geom_trimesh basic", ggplot2::ggplot() +
    geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = x, y = y)))

  vdiffr::expect_doppelganger("geom_trimesh basic with color", ggplot2::ggplot() +
                                geom_trimesh(data = df_bin_centroids,
                                             mapping = ggplot2::aes(x = x, y = y),
                                             colour = "red"))

})
