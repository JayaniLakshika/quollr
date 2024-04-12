# predict_emb() works

    Code
      predict_emb(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 75 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1       0.173        0.45     1         10
       2       0.693        0.75     2         15
       3       0.866        1.05     3         20
       4       0.173       -0.15     4          2
       5       0.173        0.45     6         10
       6       0.866        1.65     7         28
       7       0.693        0.75     8         15
       8       0.693        0.75     9         15
       9       0.693        0.75    11         15
      10       0.866        1.65    12         28
      # i 65 more rows

---

    Code
      predict_emb(test_data = s_curve_noise_test, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 25 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1       0.173       -0.15     5          2
       2       0.866        1.05    10         20
       3       0.866        1.65    13         28
       4       0.346        0.15    18          6
       5       0.346        0.15    27          6
       6       0.346        0.15    28          6
       7       0.866        1.05    29         20
       8       0.866        1.65    30         28
       9       0.173       -0.15    32          2
      10       0.520        0.45    36         11
      # i 15 more rows

# glance() works

    Code
      glance(test_data = s_curve_noise_training, prediction_df = pred_df_training,
        df_bin = df_bin, col_start = "x")
    Output
      # A tibble: 1 x 3
        Error   MSE   AIC
        <dbl> <dbl> <dbl>
      1  63.8 0.323 -453.

---

    Code
      glance(test_data = s_curve_noise_test, prediction_df = pred_df_test, df_bin = df_bin,
        col_start = "x")
    Output
      # A tibble: 1 x 3
        Error   MSE   AIC
        <dbl> <dbl> <dbl>
      1  21.8 0.346 -45.8

# augment() works

    Code
      augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = NULL, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 75 x 32
            ID      x1     x2        x3       x4       x5       x6       x7 pred_hb_id
         <int>   <dbl>  <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <int>
       1     1 -0.120  0.114  -1.99     -0.00246 -1.78e-2 -0.0181  -3.17e-3         10
       2     2 -0.0492 0.822   0.00121   0.0161   9.68e-3 -0.0834   2.30e-3         15
       3     3 -0.774  0.243   0.367    -0.0198   4.08e-3 -0.0349  -9.11e-3         20
       4     4 -0.606  1.96   -1.80      0.0132  -4.79e-4 -0.00478 -8.43e-3          2
       5     6  0.818  0.0388 -1.58      0.00253  1.67e-3  0.0781  -7.71e-3         10
       6     7  0.910  1.55    1.42      0.0124   1.60e-2 -0.00248 -8.32e-3         28
       7     8 -0.0691 0.978   0.00239   0.0115   3.50e-3  0.0898   3.59e-3         15
       8     9  0.859  1.55   -0.488    -0.00753 -1.23e-2  0.0336  -6.65e-3         15
       9    11 -0.0400 0.286   0.000801  0.0123   6.13e-3 -0.0121  -3.47e-4         15
      10    12  0.765  0.898   1.64     -0.0178   1.51e-2 -0.0710  -6.24e-3         28
      # i 65 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

---

    Code
      augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = s_curve_noise_test, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 25 x 32
            ID      x1    x2      x3       x4       x5       x6        x7 pred_hb_id
         <int>   <dbl> <dbl>   <dbl>    <dbl>    <dbl>    <dbl>     <dbl>      <int>
       1     5 -0.478   1.61 -1.88    0.0101  -0.00746  0.0101   0.00972           2
       2    10 -0.727   1.66  0.314   0.00269  0.0196   0.0559  -0.00481          20
       3    13  0.513   1.86  1.86   -0.00648 -0.0127   0.00635 -0.00770          28
       4    18  0.0635  1.48 -2.00    0.00458  0.0164  -0.0627  -0.00371           6
       5    27  0.918   1.36 -1.40    0.0161   0.0160  -0.0190   0.00341           6
       6    28  0.654   1.53 -1.76   -0.00906 -0.00186 -0.0831  -0.00323           6
       7    29 -0.397   1.86  0.0822 -0.0120  -0.00390 -0.0739   0.00251          20
       8    30  0.0891  1.33  2.00   -0.0192   0.0195  -0.0641  -0.000202         28
       9    32 -0.564   1.72 -1.83    0.0200   0.00799  0.00204  0.00273           2
      10    36  0.935   1.33 -0.647   0.00487  0.0129   0.0873   0.000686         11
      # i 15 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

