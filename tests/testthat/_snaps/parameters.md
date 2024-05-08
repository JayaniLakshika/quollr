# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 8 x 2
        hb_id mean_density
        <int>        <dbl>
      1     1        0.308
      2     2        0.115
      3     4        0.154
      4     5        0.323
      5     8        0.327
      6     9        0.173
      7    11        0.615
      8    15        0.154

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 8 x 2
        hb_id mean_density
        <int>        <dbl>
      1     1        0.308
      2     2        0.115
      3     4        0.154
      4     5        0.323
      5     8        0.327
      6     9        0.173
      7    11        0.615
      8    15        0.154

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
      integer(0)

---

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      integer(0)

