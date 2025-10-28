# comb_all_data_model() works

    Code
      comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap, model_highd = scurve_model_obj$
        model_highd, model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 1,137 x 10
              x1     x2    x3         x4        x5       x6       x7 type   emb1  emb2
           <dbl>  <dbl> <dbl>      <dbl>     <dbl>    <dbl>    <dbl> <chr> <dbl> <dbl>
       1  0.958  0.0854  1.29  0.00265    0.0171    0.0876  -2.49e-3 model    NA    NA
       2  0.818  0.116   1.56  0.00184    0.00361  -0.0318  -3.77e-3 model    NA    NA
       3  0.544  0.111   1.83 -0.00341   -0.000303  0.0196   7.04e-5 model    NA    NA
       4  0.279  0.128   1.95  0.0000880  0.00104  -0.0276  -2.27e-4 model    NA    NA
       5  0.0567 0.119   2.00  0.00733    0.00238   0.0833  -1.66e-3 model    NA    NA
       6 -0.375  1.58    1.91 -0.0126     0.000689  0.0295   9.36e-4 model    NA    NA
       7 -0.529  1.84    1.83 -0.000870  -0.000312  0.00825  1.62e-3 model    NA    NA
       8  0.935  0.364   1.31 -0.00383   -0.00292  -0.00153  8.60e-4 model    NA    NA
       9  0.705  0.449   1.70  0.00218   -0.00679  -0.0149   2.23e-3 model    NA    NA
      10  0.387  0.371   1.92  0.00876    0.00420   0.0157   2.30e-3 model    NA    NA
      # i 1,127 more rows

# comb_all_data_model_error() works

    Code
      comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
        model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$
          model_2d, error_data = model_error)
    Output
      # A tibble: 1,137 x 12
              x1     x2    x3         x4        x5       x6       x7 type   emb1  emb2
           <dbl>  <dbl> <dbl>      <dbl>     <dbl>    <dbl>    <dbl> <chr> <dbl> <dbl>
       1  0.958  0.0854  1.29  0.00265    0.0171    0.0876  -2.49e-3 model    NA    NA
       2  0.818  0.116   1.56  0.00184    0.00361  -0.0318  -3.77e-3 model    NA    NA
       3  0.544  0.111   1.83 -0.00341   -0.000303  0.0196   7.04e-5 model    NA    NA
       4  0.279  0.128   1.95  0.0000880  0.00104  -0.0276  -2.27e-4 model    NA    NA
       5  0.0567 0.119   2.00  0.00733    0.00238   0.0833  -1.66e-3 model    NA    NA
       6 -0.375  1.58    1.91 -0.0126     0.000689  0.0295   9.36e-4 model    NA    NA
       7 -0.529  1.84    1.83 -0.000870  -0.000312  0.00825  1.62e-3 model    NA    NA
       8  0.935  0.364   1.31 -0.00383   -0.00292  -0.00153  8.60e-4 model    NA    NA
       9  0.705  0.449   1.70  0.00218   -0.00679  -0.0149   2.23e-3 model    NA    NA
      10  0.387  0.371   1.92  0.00876    0.00420   0.0157   2.30e-3 model    NA    NA
      # i 1,127 more rows
      # i 2 more variables: sqrt_row_wise_total_error <dbl>, density <dbl>

