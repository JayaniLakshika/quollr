# compute_aic() works

    Code
      compute_aic(p = Inf, total, num_bins, num_obs)
    Condition
      Error in `compute_aic()`:
      ! Inf present.

---

    Code
      compute_aic(p = 0, total, num_bins, num_obs)
    Condition
      Error in `compute_aic()`:
      ! No high_D diensions.

---

    Code
      compute_aic(p = NA, total, num_bins, num_obs)
    Condition
      Error in `if (p == 0) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      compute_aic(p, total, num_bins = NA, num_obs)
    Condition
      Error in `compute_aic()`:
      ! Should assign number of non-emty bins.

---

    Code
      compute_aic(p, total, num_bins, num_obs = NA)
    Condition
      Error in `compute_aic()`:
      ! Should assign number of observations in high-D data.

---

    Code
      compute_aic(p, total = c(), num_bins, num_obs)
    Condition
      Error in `compute_aic()`:
      ! Total error is missing.

---

    Code
      compute_aic(p, total = c(1, 2, NA), num_bins, num_obs)
    Condition
      Error in `compute_aic()`:
      ! Total error vector presence NAs.

# predict_hex_id() works

    Code
      predict_hex_id(df_bin_centroids = df_bin_centroids, nldr_df_test = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP2")
    Output
      # A tibble: 75 x 4
          UMAP1    UMAP2    ID pred_hb_id
          <dbl>    <dbl> <int>      <dbl>
       1 -2.81  -3.91        1          6
       2  0.959 -0.00271     2         24
       3  1.54   0.462       3         29
       4 -2.31  -5.50        4          2
       5 -1.76  -3.46        6         12
       6  1.53   5.75        7         44
       7  0.930 -0.175       8         24
       8  0.319 -1.61        9         18
       9  1.37   0.0541     11         24
      10  1.90   4.94       12         45
      # i 65 more rows

# generate_eval_df() works

    Code
      generate_eval_df(data = s_curve_noise, prediction_df = pred_df_test,
        df_bin_centroids = df_bin_centroids, df_bin = df_bin, col_start = "x")
    Output
      # A tibble: 1 x 4
        number_of_bins number_of_observations total_error total_mse
                 <int>                  <int>       <dbl>     <dbl>
      1           3128                     75      -1685.     0.185

# predict_2d_embeddings() works

    Code
      predict_2d_embeddings(test_data = s_curve_noise_test, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 25 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <dbl>      <dbl>
       1      -1.84      -2.44       5          9
       2       1.02       0.862     10         40
       3       1.02       0.862     13         40
       4      -2.32      -1.62      18         16
       5      -1.36      -1.62      27         17
       6      -1.36      -1.62      28         17
       7       1.02       0.862     29         40
       8       1.50       1.69      30         48
       9      -1.84      -2.44      32          9
      10       0.547      0.0355    36         33
      # i 15 more rows

