# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 10 x 2
         hb_id mean_density
         <int>        <dbl>
       1     4        0.196
       2     5        0.490
       3     7        0.471
       4     8        0.363
       5     9        0.314
       6    11        0.324
       7    12        0.309
       8    15        0.569
       9    18        0.471
      10    21        1    

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 3)
    Output
      # A tibble: 10 x 2
         hb_id mean_density
         <int>        <dbl>
       1     4        0.196
       2     5        0.490
       3     7        0.471
       4     8        0.363
       5     9        0.314
       6    11        0.324
       7    12        0.309
       8    15        0.569
       9    18        0.471
      10    21        1    

# find_low_dens_hex() works

    Code
      find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = 3,
        df_bin_centroids_low = df_bin_centroids_low)
    Output
      [1] 9

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

