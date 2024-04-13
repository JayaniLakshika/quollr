test_that("geom_trimesh() works", {
  suppressMessages(num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
                             y = "UMAP2", hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3))
  num_bins_x <- num_bins_list$num_x
  num_bins_y <- num_bins_list$num_y
  suppressMessages(hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
                                         x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                         num_bins_y = num_bins_y, x_start = -0.1732051,
                                         y_start = -0.15, buffer_x = 0.346,
                                         buffer_y = 0.3, hex_size = 0.2,
                                         col_start = "UMAP"))

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)

  vdiffr::expect_doppelganger("geom_trimesh basic with bin centroids", ggplot2::ggplot() +
    geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y)))

})
