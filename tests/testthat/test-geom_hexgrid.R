test_that("geom_hexgrid() works", {
  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, r2 = r2)

  all_centroids_df <- hb_obj$centroids

  vdiffr::expect_doppelganger("geom_hexgrid basic with bin centroids", ggplot2::ggplot() +
                                geom_hexgrid(data = all_centroids_df, mapping = ggplot2::aes(x = c_x, y = c_y)))
})
