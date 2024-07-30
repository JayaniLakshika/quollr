test_that("gen_proj_langevitour() works", {
  library(ggplot2)

  r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
  num_bins_x <- 4
  hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
                        r2 = r2)
  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df) |>
    dplyr::filter(drop_empty == FALSE)
  tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
  tr_from_to_df <- gen_edges(tri_object = tr1_object)
  distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
                             start_y = "y_from", end_x = "x_to", end_y = "y_to",
                             select_vars = c("from", "to", "distance"))
  umap_data_with_hb_id <- hb_obj$data_hb_id
  df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
                             umap_data_with_hb_id)
  df_bin <- avg_highd_data(data = df_all, col_start = "x")
  ### Define type column
  df <- df_all |>
    dplyr::select(tidyselect::starts_with("x")) |>
    dplyr::mutate(type = "data") ## original dataset

  df_b <- df_bin |>
    dplyr::filter(hb_id %in% df_bin_centroids$hexID) |>
    dplyr::mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
  df_b <- df_b[match(df_bin_centroids$hexID, df_b$hb_id),] |>
    dplyr::select(-hb_id)

  df_exe <- dplyr::bind_rows(df_b, df)
  benchmark <- 0.663

  ## Set the maximum difference as the criteria
  distance_df_small_edges <- distance_df |>
    dplyr::filter(distance < benchmark) |>
    dplyr::select(-distance)

  projection_df <- cbind(
    c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
    c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

  vdiffr::expect_doppelganger("Projection from langevitour",
                              gen_proj_langevitour(points_df = df_exe, projection = projection_df,
                       edge_df = distance_df_small_edges))


})
