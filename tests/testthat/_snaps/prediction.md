# predict_emb() works

    Code
      predict_emb(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
      model_2d = scurve_model_obj$model_2d)
    Output
      # A tibble: 1,000 x 4
         pred_emb_1 pred_emb_2    ID pred_h
              <dbl>      <dbl> <int>  <int>
       1      0.234      0.896     1    215
       2      0.692      0.534     2    145
       3      0.776      0.390     3    116
       4      0.192      0.968     4    229
       5      0.192      0.968     5    229
       6      0.567      1.04      6    249
       7      0.192      0.245     7     79
       8      0.984      0.607     8    164
       9      0.776      0.823     9    206
      10      0.943      0.390    10    118
      # i 990 more rows

# glance() works

    Code
      glance(x = model_fit, highd_data = scurve)
    Output
      # A tibble: 1 x 2
        Error  RMSE
        <dbl> <dbl>
      1  881. 0.590

# augment() works

    Code
      augment(x = model_fit, highd_data = scurve)
    Output
      # A tibble: 1,000 x 32
            ID      x1    x2       x3       x4       x5       x6        x7 pred_h
         <int>   <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>     <dbl>  <int>
       1     1 -0.120  0.819 -1.99     0.0114   0.00351  0.0334   0.00638      13
       2     2 -0.0492 0.166  0.00121  0.0115  -0.0166  -0.0297   0.00509      11
       3     3 -0.774  0.651  0.367   -0.0172   0.00600  0.0211   0.00303       7
       4     4 -0.606  0.952 -1.80     0.0157  -0.00978 -0.0590  -0.00754      13
       5     5 -0.478  1.10  -1.88    -0.00423  0.00495 -0.0482  -0.00982      13
       6     6  0.818  1.78  -1.58     0.0124   0.0198   0.0560  -0.000730     14
       7     7  0.910  0.975  1.42    -0.0111   0.0132   0.0299   0.00401       5
       8     8 -0.0691 1.90   0.00239  0.0125  -0.00463  0.0260   0.00590      12
       9     9  0.859  1.34  -0.488   -0.00195 -0.0145  -0.00950  0.00593      15
      10    10 -0.727  1.56   0.314    0.0189   0.0147  -0.0659   0.00617      12
      # i 990 more rows
      # i 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
      #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
      #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
      #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
      #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
      #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, ...

# gen_diffbin1_errors() works

    Code
      gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap)
    Message
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
      v Model generated successfully!!!
    Output
            Error      RMSE b1 b2    b   m   a1   a2       d_bar
      1  628.8048 0.4098319  5  7   35  22 0.26 0.23 0.040914704
      2  562.7530 0.3672331  6  8   48  27 0.23 0.20 0.036862602
      3  520.4474 0.3364583  7  9   63  33 0.20 0.17 0.032327667
      4  430.4658 0.2722071  8 11   88  46 0.16 0.14 0.025996005
      5  406.8971 0.2544715  9 12  108  51 0.14 0.12 0.025513054
      6  380.7331 0.2388800 10 13  130  59 0.13 0.11 0.022664184
      7  329.2051 0.2014154 11 15  165  83 0.11 0.10 0.015603331
      8  321.5839 0.1977916 12 16  192  87 0.11 0.09 0.016286420
      9  306.5549 0.1846552 13 17  221  99 0.10 0.09 0.014296013
      10 276.8768 0.1678185 14 19  266 118 0.09 0.08 0.012761403
      11 273.3133 0.1632627 15 20  300 127 0.08 0.07 0.012188695
      12 256.7057 0.1539926 16 21  336 143 0.08 0.07 0.010727669
      13 243.8163 0.1448055 17 23  391 162 0.07 0.06 0.010073650
      14 230.5567 0.1363121 18 24  432 171 0.07 0.06 0.009841970
      15 223.3106 0.1314176 19 25  475 186 0.07 0.06 0.009021009
      16 214.6699 0.1266627 20 27  540 204 0.06 0.05 0.008720843
      17 210.8964 0.1245104 21 28  588 218 0.06 0.05 0.008159495
      18 208.3375 0.1225372 22 29  638 229 0.06 0.05 0.007960544
      19 191.4192 0.1131797 23 31  713 252 0.05 0.05 0.007507407
      20 187.6444 0.1098865 24 32  768 265 0.05 0.04 0.007249035
      21 188.3273 0.1110222 25 33  825 265 0.05 0.04 0.007499669
      22 181.0666 0.1083178 26 35  910 288 0.05 0.04 0.007106813
      23 177.7219 0.1072918 27 36  972 286 0.05 0.04 0.007554324
      24 178.9625 0.1076837 28 37 1036 290 0.04 0.04 0.007612155
      25 174.3416 0.1046141 29 38 1102 300 0.04 0.04 0.007522166

