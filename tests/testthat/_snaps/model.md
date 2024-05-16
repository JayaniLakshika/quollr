# fit_highd_model() works

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id      x1    x2      x3        x4         x5       x6        x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
      1     4 -0.0863 0.927 -1.76    0.00414  -0.000902  -0.0108  -0.000949
      2     8  0.760  1.19  -0.407  -0.000154  0.00342    0.0219   0.000410
      3     9 -0.110  0.935  0.0523  0.00635  -0.000537  -0.00518  0.00242 
      4    11 -0.795  1.40   0.535   0.00194  -0.0000533  0.0119   0.00173 
      5    15  0.291  1.05   1.71   -0.000141  0.00314   -0.00694 -0.00105 
      
      $df_bin_centroids
      # A tibble: 5 x 4
        hexID   c_x   c_y std_counts
        <int> <dbl> <dbl>      <dbl>
      1     4 0.157 0.242      0.962
      2     8 0.413 0.687      0.385
      3     9 0.926 0.687      0.269
      4    11 0.670 1.13       0.269
      5    15 0.926 1.58       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id      x1    x2      x3        x4         x5       x6        x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
      1     4 -0.0863 0.927 -1.76    0.00414  -0.000902  -0.0108  -0.000949
      2     8  0.760  1.19  -0.407  -0.000154  0.00342    0.0219   0.000410
      3     9 -0.110  0.935  0.0523  0.00635  -0.000537  -0.00518  0.00242 
      4    11 -0.795  1.40   0.535   0.00194  -0.0000533  0.0119   0.00173 
      5    15  0.291  1.05   1.71   -0.000141  0.00314   -0.00694 -0.00105 
      
      $df_bin_centroids
      # A tibble: 5 x 4
        hexID   c_x   c_y std_counts
        <int> <dbl> <dbl>      <dbl>
      1     4 0.157 0.242      0.962
      2     8 0.413 0.687      0.385
      3     9 0.926 0.687      0.269
      4    11 0.670 1.13       0.269
      5    15 0.926 1.58       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id      x1    x2      x3        x4         x5       x6        x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
      1     4 -0.0863 0.927 -1.76    0.00414  -0.000902  -0.0108  -0.000949
      2     8  0.760  1.19  -0.407  -0.000154  0.00342    0.0219   0.000410
      3     9 -0.110  0.935  0.0523  0.00635  -0.000537  -0.00518  0.00242 
      4    11 -0.795  1.40   0.535   0.00194  -0.0000533  0.0119   0.00173 
      5    15  0.291  1.05   1.71   -0.000141  0.00314   -0.00694 -0.00105 
      
      $df_bin_centroids
      # A tibble: 5 x 4
        hexID   c_x   c_y std_counts
        <int> <dbl> <dbl>      <dbl>
      1     4 0.173 0.187      0.962
      2     8 0.584 0.623      0.385
      3     9 0.757 0.844      0.269
      4    11 0.783 0.995      0.269
      5    15 0.842 1.57       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id      x1    x2      x3        x4         x5       x6        x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
      1     4 -0.0863 0.927 -1.76    0.00414  -0.000902  -0.0108  -0.000949
      2     8  0.760  1.19  -0.407  -0.000154  0.00342    0.0219   0.000410
      3     9 -0.110  0.935  0.0523  0.00635  -0.000537  -0.00518  0.00242 
      4    11 -0.795  1.40   0.535   0.00194  -0.0000533  0.0119   0.00173 
      5    15  0.291  1.05   1.71   -0.000141  0.00314   -0.00694 -0.00105 
      
      $df_bin_centroids
      # A tibble: 5 x 4
        hexID   c_x   c_y std_counts
        <int> <dbl> <dbl>      <dbl>
      1     4 0.173 0.187      0.962
      2     8 0.584 0.623      0.385
      3     9 0.757 0.844      0.269
      4    11 0.783 0.995      0.269
      5    15 0.842 1.57       1    
      

