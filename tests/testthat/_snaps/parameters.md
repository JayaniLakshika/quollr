# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 9 x 2
        hb_id mean_density
        <int>        <dbl>
      1     1        0.5  
      2     2        0.344
      3     4        0.354
      4     5        0.538
      5     8        0.703
      6     9        0.344
      7    11        0.438
      8    14        0.812
      9    15        0.406

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 9 x 2
        hb_id mean_density
        <int>        <dbl>
      1     1        0.5  
      2     2        0.344
      3     4        0.354
      4     5        0.538
      5     8        0.703
      6     9        0.344
      7    11        0.438
      8    14        0.812
      9    15        0.406

# find_low_dens_hex() works

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      [1] 2

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)))
    Output
      [1] 2

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      [1] 2

