# find_benchmark_value() works

    Code
      find_benchmark_value(distance_df, distance)
    Condition
      Error in `find_benchmark_value()`:
      ! NAs present

---

    Code
      find_benchmark_value(data_dist, dist)
    Condition
      Warning in `find_benchmark_value()`:
      Difference between unique distances are increasing. Please use a suitable value for benchmark.
    Output
      [1] NA

# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = num_bins_x)
    Output
      # A tibble: 16 x 6
              x      y hexID counts std_counts mean_density
          <dbl>  <dbl> <int>  <int>      <dbl>        <dbl>
       1 -3.27  -5.74      1      2      0.222        0.722
       2 -1.84  -5.74      2      6      0.667        0.519
       3 -2.55  -4.38      6      7      0.778        0.417
       4 -1.12  -4.38      7      5      0.556        0.472
       5 -3.27  -3.01     11      2      0.222        0.519
       6 -1.84  -3.01     12      2      0.222        0.278
       7 -0.407 -3.01     13      2      0.222        0.407
       8 -1.12  -1.65     17      1      0.111        0.444
       9  0.308 -1.65     18      8      0.889        0.167
      10  1.02  -0.280    24      7      0.778        0.389
      11  0.308  1.09     28      1      0.111        0.722
      12  1.74   1.09     29      6      0.667        0.444
      13  0.308  3.82     38      2      0.222        0.889
      14  1.74   3.82     39      8      0.889        0.611
      15  1.02   5.18     44      9      1            0.833
      16  2.46   5.18     45      7      0.778        1    

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = NA)
    Condition
      Error in `compute_mean_density_hex()`:
      ! Number of bins along x axis is not defined.

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids_na, num_bins_x = num_bins_x,
        col_std_counts = std_counts_new)
    Condition
      Error in `compute_mean_density_hex()`:
      ! NAs present

# find_low_density_hexagons() works

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts")
    Output
      [1] 12 13

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)),
        col_std_counts = "std_counts")
    Output
      [1] 12 13

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = NA,
        df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts")
    Condition
      Error in `find_low_density_hexagons()`:
      ! Number of bins along x axis is not defined.

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids_na,
        num_bins_x = num_bins_x, df_bin_centroids_low = df_bin_centroids_low,
        col_std_counts = "std_counts_new")
    Condition
      Error in `find_low_density_hexagons()`:
      ! NAs present

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts")
    Message
      Don't need to remove low-density hexagonal bins.
    Output
      NULL

