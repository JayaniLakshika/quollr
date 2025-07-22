# comb_all_data_model() works

    Code
      comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap, model_highd = scurve_model_obj$
        model_highd, model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 1,026 x 10
               x1    x2    x3        x4        x5       x6        x7 type   emb1  emb2
            <dbl> <dbl> <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <chr> <dbl> <dbl>
       1 -0.529   1.84   1.83 -0.000870 -0.000312  0.00825  0.00162  model    NA    NA
       2  0.935   0.364  1.31 -0.00383  -0.00292  -0.00153  0.000860 model    NA    NA
       3 -0.00776 0.305  2.00 -0.000357  0.000463  0.00485  0.00122  model    NA    NA
       4 -0.254   0.261  1.96 -0.00457   0.00248  -0.0156   0.000782 model    NA    NA
       5 -0.706   1.38   1.70  0.000786 -0.00266  -0.0132  -0.00169  model    NA    NA
       6 -0.839   1.77   1.54  0.00281   0.000455  0.0260  -0.00147  model    NA    NA
       7  0.182   0.691  1.98 -0.00137   0.00421   0.00362  0.000414 model    NA    NA
       8 -0.567   0.369  1.81 -0.00213  -0.000838 -0.0130   0.000421 model    NA    NA
       9 -0.724   0.698  1.68 -0.000712  0.00110  -0.00523 -0.000711 model    NA    NA
      10 -0.992   0.960  1.07 -0.00316   0.00512  -0.00440  0.00123  model    NA    NA
      # i 1,016 more rows

# comb_all_data_model_error() works

    Code
      comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
        model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$
          model_2d, error_data = model_error)
    Output
      # A tibble: 1,026 x 12
               x1    x2    x3        x4        x5       x6        x7 type   emb1  emb2
            <dbl> <dbl> <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <chr> <dbl> <dbl>
       1 -0.529   1.84   1.83 -0.000870 -0.000312  0.00825  0.00162  model    NA    NA
       2  0.935   0.364  1.31 -0.00383  -0.00292  -0.00153  0.000860 model    NA    NA
       3 -0.00776 0.305  2.00 -0.000357  0.000463  0.00485  0.00122  model    NA    NA
       4 -0.254   0.261  1.96 -0.00457   0.00248  -0.0156   0.000782 model    NA    NA
       5 -0.706   1.38   1.70  0.000786 -0.00266  -0.0132  -0.00169  model    NA    NA
       6 -0.839   1.77   1.54  0.00281   0.000455  0.0260  -0.00147  model    NA    NA
       7  0.182   0.691  1.98 -0.00137   0.00421   0.00362  0.000414 model    NA    NA
       8 -0.567   0.369  1.81 -0.00213  -0.000838 -0.0130   0.000421 model    NA    NA
       9 -0.724   0.698  1.68 -0.000712  0.00110  -0.00523 -0.000711 model    NA    NA
      10 -0.992   0.960  1.07 -0.00316   0.00512  -0.00440  0.00123  model    NA    NA
      # i 1,016 more rows
      # i 2 more variables: sqrt_row_wise_total_error <dbl>, density <dbl>

