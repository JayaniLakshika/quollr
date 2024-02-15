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
               <dbl>       <dbl> <int>      <int>
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

