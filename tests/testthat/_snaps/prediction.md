# predict_emb() works

    Code
      predict_emb(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 3,750 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1      0.227       1.04       1         18
       2      1.04        0.189      2          8
       3      0.717       0.189      3          7
       4      0.0634      0.755      5         13
       5      0.390       0.755      6         14
       6     -0.1        -0.0938     7          1
       7      0.717       0.755      9         15
       8      1.04        0.189     10          8
       9      0.880       0.472     11         12
      10      0.0634      0.189     12          5
      # i 3,740 more rows

---

    Code
      predict_emb(test_data = s_curve_noise_test, df_bin_centroids = df_bin_centroids,
        df_bin = df_bin, type_NLDR = "UMAP")
    Output
      # A tibble: 1,250 x 4
         pred_UMAP_1 pred_UMAP_2    ID pred_hb_id
               <dbl>       <dbl> <int>      <int>
       1      0.0634       0.755     4         13
       2      1.04         0.189     8          8
       3      0.0634       0.189    13          5
       4      0.717        0.755    14         15
       5      0.227        1.04     17         18
       6      0.390        0.189    20          6
       7      0.227        1.04     21         18
       8      0.390        0.189    22          6
       9      0.717        0.755    23         15
      10      0.390        0.755    28         14
      # i 1,240 more rows

# glance() works

    Code
      glance(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = NULL, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 1 x 2
        Error   MSE
        <dbl> <dbl>
      1 2907. 0.275

---

    Code
      glance(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = s_curve_noise_test, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 1 x 2
        Error   MSE
        <dbl> <dbl>
      1  982. 0.276

# augment() works

    Code
      augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin, training_data = s_curve_noise_training,
        newdata = NULL, type_NLDR = "UMAP", col_start = "x")
    Output
      # A tibble: 3,750 x 32
            ID      x1     x2        x3       x4       x5       x6       x7 pred_hb_id
         <int>   <dbl>  <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <int>
       1     1 -0.120  1.64   -1.99      0.0104   0.0125   0.0923  -1.28e-3         18
       2     2 -0.0492 1.51    0.00121  -0.0177   0.00726 -0.0362  -5.35e-3          8
       3     3 -0.774  1.30    0.367    -0.00173  0.0156  -0.0962   3.35e-3          7
       4     5 -0.478  0.0177 -1.88      0.00848  0.00533  0.0998   6.77e-4         13
       5     6  0.818  0.927  -1.58     -0.00318 -0.00980  0.0989   6.96e-3         14
       6     7  0.910  1.40    1.42      0.00699 -0.0182  -0.0710   9.66e-3          1
       7     9  0.859  1.59   -0.488    -0.0119   0.00421 -0.00440 -5.95e-3         15
       8    10 -0.727  1.62    0.314     0.00251  0.0177  -0.0755  -3.69e-3          8
       9    11 -0.0400 1.23    0.000801 -0.00489  0.00570  0.0722  -7.89e-3         12
      10    12  0.765  0.510   1.64     -0.00641 -0.00941 -0.0708   9.89e-3          5
      # i 3,740 more rows
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
      # A tibble: 1,250 x 32
            ID      x1    x2       x3       x4       x5      x6        x7 pred_hb_id
         <int>   <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl>      <int>
       1     4 -0.606  0.246 -1.80    -0.00897 -0.0187  -0.0716  0.00126          13
       2     8 -0.0691 1.59   0.00239  0.0127  -0.0130   0.0396 -0.000185          8
       3    13  0.513  1.02   1.86     0.0141  -0.0149   0.0619 -0.00309           5
       4    14  0.869  0.576 -0.505   -0.0196   0.00169 -0.0197  0.00597          15
       5    17 -0.737  1.94  -1.68     0.00601 -0.0113   0.0301 -0.00988          18
       6    20 -0.795  0.488  1.61    -0.0126   0.0131  -0.0956 -0.00283           6
       7    21  0.325  1.61  -1.95    -0.0165   0.00487  0.0951 -0.00758          18
       8    22 -0.717  1.22   1.70     0.00474  0.00667  0.0439  0.00740           6
       9    23  0.923  1.32  -0.616    0.0155   0.0174   0.0707 -0.00160          15
      10    28  0.654  0.424 -1.76     0.00721 -0.00188  0.0982 -0.00947          14
      # i 1,240 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

