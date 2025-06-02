# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 15 x 2
         hb_id mean_density
         <int>        <dbl>
       1     1        0.610
       2     2        0.394
       3     3        0.375
       4     5        0.397
       5     6        0.393
       6     7        0.337
       7     8        0.555
       8    11        0.785
       9    12        0.360
      10    13        0.383
      11    14        0.556
      12    15        0.390
      13    17        0.567
      14    18        0.464
      15    19        0.705

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 15 x 2
         hb_id mean_density
         <int>        <dbl>
       1     1        0.610
       2     2        0.394
       3     3        0.375
       4     5        0.397
       5     6        0.393
       6     7        0.337
       7     8        0.555
       8    11        0.785
       9    12        0.360
      10    13        0.383
      11    14        0.556
      12    15        0.390
      13    17        0.567
      14    18        0.464
      15    19        0.705

# find_low_dens_hex() works

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      integer(0)

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

