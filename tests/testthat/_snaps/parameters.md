# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 5 x 2
        hb_id mean_density
        <int>        <dbl>
      1     2        1    
      2     6        0.5  
      3    11        0.815
      4    14        0.630
      5    15        0.426

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, bin1 = 4)
    Output
      # A tibble: 5 x 2
        hb_id mean_density
        <int>        <dbl>
      1     2        1    
      2     6        0.5  
      3    11        0.815
      4    14        0.630
      5    15        0.426

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

