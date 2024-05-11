# predict_emb() works

    Code
      predict_emb(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 75 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1       0.328       0.539     1          3
       2       0.328       0.539     2          3
       3       0.755       1.28      3          6
       4      -0.1        -0.202     4          1
       5       0.328       0.539     6          3
       6       1.18        2.02      7          8
       7       0.328       0.539     8          3
       8       0.328       0.539     9          3
       9       0.328       0.539    11          3
      10       1.18        2.02     12          8
      # i 65 more rows

---

    Code
      predict_emb(test_data = s_curve_noise_test, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 25 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1      -0.1        -0.202     5          1
       2       0.755       1.28     10          6
       3       0.755       1.28     13          6
       4      -0.1        -0.202    18          1
       5       0.328       0.539    27          3
       6       0.328       0.539    28          3
       7       0.755       1.28     29          6
       8       0.755       1.28     30          6
       9      -0.1        -0.202    32          1
      10       0.328       0.539    36          3
      # i 15 more rows

# glance() works

    Code
      glance(test_data = s_curve_noise_training, prediction_df = pred_df_training,
        df_bin = df_bin, col_start = "x")
    Output
      # A tibble: 1 x 2
        Error   MSE
        <dbl> <dbl>
      1  116.  1.04

---

    Code
      glance(test_data = s_curve_noise_test, prediction_df = pred_df_test, df_bin = df_bin,
        col_start = "x")
    Output
      # A tibble: 1 x 2
        Error   MSE
        <dbl> <dbl>
      1  36.3 0.885

# augment() works

    Code
      augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = NULL, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 75 x 32
            ID      x1     x2        x3       x4       x5       x6       x7 pred_hb_id
         <int>   <dbl>  <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <int>
       1     1 -0.120  0.114  -1.99     -0.00246 -1.78e-2 -0.0181  -3.17e-3          3
       2     2 -0.0492 0.822   0.00121   0.0161   9.68e-3 -0.0834   2.30e-3          3
       3     3 -0.774  0.243   0.367    -0.0198   4.08e-3 -0.0349  -9.11e-3          6
       4     4 -0.606  1.96   -1.80      0.0132  -4.79e-4 -0.00478 -8.43e-3          1
       5     6  0.818  0.0388 -1.58      0.00253  1.67e-3  0.0781  -7.71e-3          3
       6     7  0.910  1.55    1.42      0.0124   1.60e-2 -0.00248 -8.32e-3          8
       7     8 -0.0691 0.978   0.00239   0.0115   3.50e-3  0.0898   3.59e-3          3
       8     9  0.859  1.55   -0.488    -0.00753 -1.23e-2  0.0336  -6.65e-3          3
       9    11 -0.0400 0.286   0.000801  0.0123   6.13e-3 -0.0121  -3.47e-4          3
      10    12  0.765  0.898   1.64     -0.0178   1.51e-2 -0.0710  -6.24e-3          8
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
       1     5 -0.478   1.61 -1.88    0.0101  -0.00746  0.0101   0.00972           1
       2    10 -0.727   1.66  0.314   0.00269  0.0196   0.0559  -0.00481           6
       3    13  0.513   1.86  1.86   -0.00648 -0.0127   0.00635 -0.00770           6
       4    18  0.0635  1.48 -2.00    0.00458  0.0164  -0.0627  -0.00371           1
       5    27  0.918   1.36 -1.40    0.0161   0.0160  -0.0190   0.00341           3
       6    28  0.654   1.53 -1.76   -0.00906 -0.00186 -0.0831  -0.00323           3
       7    29 -0.397   1.86  0.0822 -0.0120  -0.00390 -0.0739   0.00251           6
       8    30  0.0891  1.33  2.00   -0.0192   0.0195  -0.0641  -0.000202          6
       9    32 -0.564   1.72 -1.83    0.0200   0.00799  0.00204  0.00273           1
      10    36  0.935   1.33 -0.647   0.00487  0.0129   0.0873   0.000686          3
      # i 15 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

