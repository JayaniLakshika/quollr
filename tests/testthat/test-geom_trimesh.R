test_that("geom_trimesh() works", {
  num_bins_x <- suppressMessages(calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                           x = "UMAP1", hex_size = NA, buffer_x = NA))
  num_bins_y <- suppressMessages(calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                           y = "UMAP2", hex_size = NA, buffer_y = NA))

  ## Obtain the hexbin object
  hb_obj <- suppressMessages(generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
                                      x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                      num_bins_y = num_bins_y, x_start = NA,
                                      y_start = NA, buffer_x = NA,
                                      buffer_y = NA, hex_size = NA))

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$full_grid_hex_centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$hex_id_with_std_counts))
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)
  vdiffr::expect_doppelganger("geom_trimesh basic with bin centroids", ggplot2::ggplot() +
    geom_trimesh(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y)))

  # vdiffr::expect_doppelganger("geom_trimesh basic with color", ggplot2::ggplot() +
  #                               geom_trimesh(data = df_bin_centroids,
  #                                            mapping = ggplot2::aes(x = c_x, y = c_y),
  #                                            colour = "red"))
  #
  # umap_with_hb_id <- as.data.frame(do.call(cbind, hb_obj$nldr_data_with_hex_id))
  # df_bin_mean <- extract_hexbin_mean(nldr_df_with_hex_id = umap_with_hb_id,  counts_df = counts_df)
  #
  # vdiffr::expect_doppelganger("geom_trimesh basic with bin mean", ggplot2::ggplot() +
  #                               geom_trimesh(data = df_bin_mean, mapping = ggplot2::aes(x = c_x, y = c_y)))
  #
  # vdiffr::expect_doppelganger("geom_trimesh basic with color", ggplot2::ggplot() +
  #                               geom_trimesh(data = df_bin_mean,
  #                                            mapping = ggplot2::aes(x = c_x, y = c_y),
  #                                            colour = "red"))

})
