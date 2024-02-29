# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = num_bins_x)
    Output
      $hb_id
       [1]  2  6  7 12 13 18 24 28 29 34
      
      $mean_density
       [1] 0.4117647 0.2941176 0.3235294 0.5000000 0.4313725 0.3333333 0.4705882
       [8] 0.6176471 0.5882353 0.2647059
      

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = NA)
    Condition
      Error in `compute_mean_density_hex()`:
      ! Number of bins along x axis is not defined.

# find_low_dens_hex() works

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      [1] 7

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)))
    Output
      numeric(0)

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      numeric(0)

