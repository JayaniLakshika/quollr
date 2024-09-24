# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 11 x 2
         hb_id mean_density
         <int>        <dbl>
       1     5        0.286
       2     6        0.714
       3     9        0.690
       4    10        0.443
       5    14        0.393
       6    15        0.619
       7    19        0.357
       8    23        0.690
       9    27        0.429
      10    28        0.464
      11    31        0.643

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 11 x 2
         hb_id mean_density
         <int>        <dbl>
       1     5        0.286
       2     6        0.714
       3     9        0.690
       4    10        0.443
       5    14        0.393
       6    15        0.619
       7    19        0.357
       8    23        0.690
       9    27        0.429
      10    28        0.464
      11    31        0.643

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

