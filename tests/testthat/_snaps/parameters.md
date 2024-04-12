# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = num_bins_x)
    Output
      # A tibble: 10 x 2
         hb_id mean_density
         <int>        <dbl>
       1     2        0.412
       2     5        0.294
       3     6        0.324
       4    10        0.5  
       5    11        0.431
       6    15        0.333
       7    20        0.471
       8    23        0.618
       9    24        0.588
      10    28        0.265

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
      [1] 6

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)))
    Output
      integer(0)

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      integer(0)

