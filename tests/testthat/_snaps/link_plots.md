# comb_all_data_model() works

    Code
      comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap, model_highd = scurve_model_obj$
        model_highd, model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 5,122 x 10
              x1    x2    x3         x4        x5       x6        x7 type   emb1  emb2
           <dbl> <dbl> <dbl>      <dbl>     <dbl>    <dbl>     <dbl> <chr> <dbl> <dbl>
       1 -0.992   1.91 1.11  -0.000427   0.000624  0.00749  0.00105  model    NA    NA
       2 -0.906   1.93 1.41  -0.0000183  0.00331  -0.0204  -0.000363 model    NA    NA
       3 -0.680   1.93 1.72  -0.000810  -0.00259  -0.00449  0.00153  model    NA    NA
       4  0.461   1.93 1.89  -0.00478    0.00492   0.00835  0.00172  model    NA    NA
       5 -0.985   1.75 0.853 -0.00202    0.000397  0.00331  0.000338 model    NA    NA
       6 -0.980   1.66 1.17  -0.000374  -0.00154   0.0165   0.000126 model    NA    NA
       7 -0.821   1.64 1.56  -0.000459   0.000538 -0.0123   0.000780 model    NA    NA
       8 -0.484   1.68 1.87   0.00313    0.00241   0.00823 -0.00117  model    NA    NA
       9 -0.0991  1.70 1.99   0.00103    0.00150   0.00877 -0.000193 model    NA    NA
      10  0.295   1.74 1.95  -0.00165    0.000459  0.00330  0.000257 model    NA    NA
      # i 5,112 more rows

# comb_all_data_model_error() works

    Code
      comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
        model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$
          model_2d, error_data = model_error)
    Output
      # A tibble: 5,122 x 12
              x1    x2    x3         x4        x5       x6        x7 type   emb1  emb2
           <dbl> <dbl> <dbl>      <dbl>     <dbl>    <dbl>     <dbl> <chr> <dbl> <dbl>
       1 -0.992   1.91 1.11  -0.000427   0.000624  0.00749  0.00105  model    NA    NA
       2 -0.906   1.93 1.41  -0.0000183  0.00331  -0.0204  -0.000363 model    NA    NA
       3 -0.680   1.93 1.72  -0.000810  -0.00259  -0.00449  0.00153  model    NA    NA
       4  0.461   1.93 1.89  -0.00478    0.00492   0.00835  0.00172  model    NA    NA
       5 -0.985   1.75 0.853 -0.00202    0.000397  0.00331  0.000338 model    NA    NA
       6 -0.980   1.66 1.17  -0.000374  -0.00154   0.0165   0.000126 model    NA    NA
       7 -0.821   1.64 1.56  -0.000459   0.000538 -0.0123   0.000780 model    NA    NA
       8 -0.484   1.68 1.87   0.00313    0.00241   0.00823 -0.00117  model    NA    NA
       9 -0.0991  1.70 1.99   0.00103    0.00150   0.00877 -0.000193 model    NA    NA
      10  0.295   1.74 1.95  -0.00165    0.000459  0.00330  0.000257 model    NA    NA
      # i 5,112 more rows
      # i 2 more variables: sqrt_row_wise_total_error <dbl>, density <dbl>

