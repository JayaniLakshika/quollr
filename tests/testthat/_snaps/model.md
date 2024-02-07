# fit_high_d_model() works

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = NA, shape_val = NA,
        is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = NA,
        is_avg_high_d = TRUE, column_start_text = "x")
    Output
      $df_bin
      # A tibble: 11 x 8
         hb_id     x1    x2     x3        x4         x5        x6         x7
         <int>  <dbl> <dbl>  <dbl>     <dbl>      <dbl>     <dbl>      <dbl>
       1     1 -0.337 0.186 -1.92   0.00180   0.00255   -0.0445   -0.000892 
       2     8 -0.515 0.989 -1.69   0.00344   0.00290   -0.0167    0.0000764
       3     9 -0.302 1.55  -1.90   0.00763  -0.00494   -0.0511   -0.00819  
       4    16 -0.214 1.45  -1.98  -0.00966  -0.00370    0.0573    0.000307 
       5    17  0.515 1.21  -1.80   0.0121   -0.00690    0.00531  -0.00141  
       6    24  0.958 0.729 -1.16   0.00110  -0.00509    0.0420    0.00307  
       7    32  0.659 0.628 -0.252  0.000870  0.0127     0.0156   -0.00207  
       8    33  0.772 1.55  -0.411 -0.00364   0.000835   0.0267    0.000950 
       9    40 -0.123 1.52   1.04   0.00686   0.0000898 -0.0208    0.00369  
      10    48 -0.130 0.874  1.22  -0.00244   0.00388    0.0171   -0.00266  
      11    55  0.660 0.512  1.58  -0.00437   0.00270    0.000203 -0.00344  
      
      $df_bin_centroids
      # A tibble: 11 x 5
              x       y hexID counts std_counts
          <dbl>   <dbl> <int>  <int>      <dbl>
       1 -3.27  -3.27       1      5     0.278 
       2 -2.79  -2.44       8      9     0.5   
       3 -1.84  -2.44       9      2     0.111 
       4 -2.32  -1.62      16      1     0.0556
       5 -1.36  -1.62      17      6     0.333 
       6 -0.885 -0.791     24      3     0.167 
       7 -0.407  0.0355    32      3     0.167 
       8  0.547  0.0355    33      6     0.333 
       9  1.02   0.862     40     18     1     
      10  1.50   1.69      48     14     0.778 
      11  1.98   2.51      55      8     0.444 
      

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = NA,
        is_avg_high_d = TRUE, column_start_text = "x")
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1    x2     x3         x4        x5       x6        x7
        <int>   <dbl> <dbl>  <dbl>      <dbl>     <dbl>    <dbl>     <dbl>
      1     1 -0.337  0.186 -1.92   0.00180    0.00255  -0.0445  -0.000892
      2     6 -0.524  1.09  -1.70   0.00441    0.00256  -0.0155  -0.000774
      3    12  0.301  1.28  -1.88   0.00688   -0.00579  -0.00826 -0.00373 
      4    17  0.886  0.810 -1.43   0.000758  -0.00680   0.0547   0.00487 
      5    23  0.799  0.527 -0.476  0.00328    0.00490  -0.00246 -0.000786
      6    28  0.743  1.47  -0.377 -0.00162    0.00279   0.0323   0.000922
      7    34 -0.0726 1.51   1.03   0.00682    0.000708 -0.0219   0.00362 
      8    39 -0.269  0.987  1.13   0.0000662  0.00230   0.0267  -0.00184 
      9    45  0.597  0.514  1.63  -0.00624    0.00356  -0.0108  -0.00360 
      
      $df_bin_centroids
      # A tibble: 9 x 5
             x      y hexID counts std_counts
         <dbl>  <dbl> <int>  <int>      <dbl>
      1 -3.27  -3.27      1      5      0.294
      2 -2.55  -2.59      6     10      0.588
      3 -1.84  -1.92     12      7      0.412
      4 -1.12  -1.24     17      3      0.176
      5 -0.407 -0.565    23      3      0.176
      6  0.308  0.111    28      7      0.412
      7  1.02   0.787    34     17      1    
      8  1.74   1.46     39     13      0.765
      9  2.46   2.14     45     10      0.588
      

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = NA,
        is_avg_high_d = TRUE, column_start_text = "x")
    Condition
      Warning in `compute_mean_density_hex()`:
      There are hexagonal bins that don't have any neighbouring bins.
    Message
      Don't need to remove low-density hexagonal bins.
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1    x2     x3         x4        x5       x6        x7
        <int>   <dbl> <dbl>  <dbl>      <dbl>     <dbl>    <dbl>     <dbl>
      1     1 -0.337  0.186 -1.92   0.00180    0.00255  -0.0445  -0.000892
      2     6 -0.524  1.09  -1.70   0.00441    0.00256  -0.0155  -0.000774
      3    12  0.301  1.28  -1.88   0.00688   -0.00579  -0.00826 -0.00373 
      4    17  0.886  0.810 -1.43   0.000758  -0.00680   0.0547   0.00487 
      5    23  0.799  0.527 -0.476  0.00328    0.00490  -0.00246 -0.000786
      6    28  0.743  1.47  -0.377 -0.00162    0.00279   0.0323   0.000922
      7    34 -0.0726 1.51   1.03   0.00682    0.000708 -0.0219   0.00362 
      8    39 -0.269  0.987  1.13   0.0000662  0.00230   0.0267  -0.00184 
      9    45  0.597  0.514  1.63  -0.00624    0.00356  -0.0108  -0.00360 
      
      $df_bin_centroids
      # A tibble: 9 x 5
             x      y hexID counts std_counts
         <dbl>  <dbl> <int>  <int>      <dbl>
      1 -3.27  -3.27      1      5      0.294
      2 -2.55  -2.59      6     10      0.588
      3 -1.84  -1.92     12      7      0.412
      4 -1.12  -1.24     17      3      0.176
      5 -0.407 -0.565    23      3      0.176
      6  0.308  0.111    28      7      0.412
      7  1.02   0.787    34     17      1    
      8  1.74   1.46     39     13      0.765
      9  2.46   2.14     45     10      0.588
      

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0.2,
        is_avg_high_d = TRUE, column_start_text = "x")
    Condition
      Warning in `compute_mean_density_hex()`:
      There are hexagonal bins that don't have any neighbouring bins.
    Message
      Don't need to remove low-density hexagonal bins.
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1    x2     x3         x4        x5       x6        x7
        <int>   <dbl> <dbl>  <dbl>      <dbl>     <dbl>    <dbl>     <dbl>
      1     1 -0.337  0.186 -1.92   0.00180    0.00255  -0.0445  -0.000892
      2     6 -0.524  1.09  -1.70   0.00441    0.00256  -0.0155  -0.000774
      3    12  0.301  1.28  -1.88   0.00688   -0.00579  -0.00826 -0.00373 
      4    17  0.886  0.810 -1.43   0.000758  -0.00680   0.0547   0.00487 
      5    23  0.799  0.527 -0.476  0.00328    0.00490  -0.00246 -0.000786
      6    28  0.743  1.47  -0.377 -0.00162    0.00279   0.0323   0.000922
      7    34 -0.0726 1.51   1.03   0.00682    0.000708 -0.0219   0.00362 
      8    39 -0.269  0.987  1.13   0.0000662  0.00230   0.0267  -0.00184 
      9    45  0.597  0.514  1.63  -0.00624    0.00356  -0.0108  -0.00360 
      
      $df_bin_centroids
      # A tibble: 9 x 5
             x      y hexID counts std_counts
         <dbl>  <dbl> <int>  <int>      <dbl>
      1 -3.27  -3.27      1      5      0.294
      2 -2.55  -2.59      6     10      0.588
      3 -1.84  -1.92     12      7      0.412
      4 -1.12  -1.24     17      3      0.176
      5 -0.407 -0.565    23      3      0.176
      6  0.308  0.111    28      7      0.412
      7  1.02   0.787    34     17      1    
      8  1.74   1.46     39     13      0.765
      9  2.46   2.14     45     10      0.588
      

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 1.5,
        is_avg_high_d = TRUE, column_start_text = "x")
    Condition
      Error in `fit_high_d_model()`:
      ! Benchmark value to remove low density hexagons is too large.

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0,
        is_avg_high_d = TRUE, column_start_text = "x")
    Condition
      Error in `fit_high_d_model()`:
      ! Benchmark value to remove low density hexagons is too small.

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = 0.2,
        is_avg_high_d = TRUE, column_start_text = "x")
    Condition
      Error in `fit_high_d_model()`:
      ! Need to initialise `is_rm_lwd_hex = TRUE`.

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = NA,
        is_avg_high_d = TRUE, column_start_text = "x")
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1    x2     x3         x4        x5       x6        x7
        <int>   <dbl> <dbl>  <dbl>      <dbl>     <dbl>    <dbl>     <dbl>
      1     1 -0.337  0.186 -1.92   0.00180    0.00255  -0.0445  -0.000892
      2     6 -0.524  1.09  -1.70   0.00441    0.00256  -0.0155  -0.000774
      3    12  0.301  1.28  -1.88   0.00688   -0.00579  -0.00826 -0.00373 
      4    17  0.886  0.810 -1.43   0.000758  -0.00680   0.0547   0.00487 
      5    23  0.799  0.527 -0.476  0.00328    0.00490  -0.00246 -0.000786
      6    28  0.743  1.47  -0.377 -0.00162    0.00279   0.0323   0.000922
      7    34 -0.0726 1.51   1.03   0.00682    0.000708 -0.0219   0.00362 
      8    39 -0.269  0.987  1.13   0.0000662  0.00230   0.0267  -0.00184 
      9    45  0.597  0.514  1.63  -0.00624    0.00356  -0.0108  -0.00360 
      
      $df_bin_centroids
      # A tibble: 9 x 5
        hexID      x     y counts std_counts
        <int>  <dbl> <dbl>  <int>      <dbl>
      1     1 -3.07  -3.95      5      0.294
      2     6 -2.58  -4.90     10      0.588
      3    12 -1.77  -4.72      7      0.412
      4    17 -1.15  -3.52      3      0.176
      5    23 -0.458 -1.95      3      0.176
      6    28  0.304 -1.43      7      0.412
      7    34  0.925  2.61     17      1    
      8    39  1.44   2.85     13      0.765
      9    45  2.18   4.68     10      0.588
      

---

    Code
      fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap,
        x = "UMAP1", y = "UMAP1", cell_area = 1, num_bins_x = 4, shape_val = 1.833091,
        is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = NA,
        is_avg_high_d = FALSE, column_start_text = "x")
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id     x1    x2     x3        x4        x5      x6        x7
        <int>  <dbl> <dbl>  <dbl>     <dbl>     <dbl>   <dbl>     <dbl>
      1     1 -0.390 0.188 -1.90  -0.00299  -0.00278  -0.0482  0.00145 
      2     6 -0.618 1.18  -1.63   0.00406   0.00266  -0.0163 -0.000316
      3    12  0.321 1.42  -1.89   0.00840  -0.00784  -0.0260 -0.00236 
      4    17  0.896 0.899 -1.42   0.000510 -0.00758   0.0645  0.00203 
      5    23  0.765 0.495 -0.408 -0.000445  0.00756  -0.0118 -0.00243 
      6    28  0.761 1.58  -0.394 -0.00209   0.000140  0.0249 -0.00122 
      7    34 -0.169 1.53   1.12   0.00684   0.000846 -0.0191  0.00333 
      8    39 -0.476 0.878  1.32   0.000167 -0.000365  0.0219 -0.00143 
      9    45  0.626 0.443  1.69  -0.00989   0.00645  -0.0132 -0.00497 
      
      $df_bin_centroids
      # A tibble: 9 x 5
        hexID      x     y counts std_counts
        <int>  <dbl> <dbl>  <int>      <dbl>
      1     1 -3.07  -3.95      5      0.294
      2     6 -2.58  -4.90     10      0.588
      3    12 -1.77  -4.72      7      0.412
      4    17 -1.15  -3.52      3      0.176
      5    23 -0.458 -1.95      3      0.176
      6    28  0.304 -1.43      7      0.412
      7    34  0.925  2.61     17      1    
      8    39  1.44   2.85     13      0.765
      9    45  2.18   4.68     10      0.588
      

