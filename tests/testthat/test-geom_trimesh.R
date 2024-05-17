test_that("geom_trimesh() works", {

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = 3, r2 = r2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df) |>
    dplyr::filter(drop_empty == FALSE)

  vdiffr::expect_doppelganger("geom_trimesh basic with bin centroids", ggplot2::ggplot() +
    geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y)))

})
