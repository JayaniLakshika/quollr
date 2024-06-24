# fit_highd_model() works

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 10 x 8
         hb_id      x1     x2      x3        x4         x5       x6         x7
         <int>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>    <dbl>      <dbl>
       1     4 -0.472  1.07   -1.77    0.00384   0.000797  -0.0288  -0.00176  
       2     5  0.491  1.51   -1.86    0.0114   -0.0106    -0.0291  -0.000367 
       3     7 -0.175  0.0562 -1.98    0.00204  -0.00179    0.0112  -0.00111  
       4     8  0.760  0.510  -1.56    0.00155   0.000204   0.0451   0.00120  
       5     9  0.904  1.39   -0.596  -0.00257  -0.00546    0.0472   0.00125  
       6    11  0.626  0.785  -0.227  -0.000563  0.0126     0.0162   0.000637 
       7    12 -0.0243 1.04    0.0232  0.00725   0.000935  -0.0148   0.00153  
       8    15 -0.795  1.40    0.535   0.00194  -0.0000533  0.0119   0.00173  
       9    18  0.113  0.745   1.76   -0.00156   0.00305   -0.00738 -0.00162  
      10    21  0.628  1.63    1.61    0.00253   0.00332   -0.00610  0.0000244
      
      $df_bin_centroids
      # A tibble: 10 x 5
         hexID     c_x   c_y std_counts drop_empty
         <int>   <dbl> <dbl>      <dbl> <lgl>     
       1     4  0.0832 0.115      0.882 FALSE     
       2     5  0.450  0.115      0.176 FALSE     
       3     7 -0.1    0.433      0.118 FALSE     
       4     8  0.266  0.433      0.294 FALSE     
       5     9  0.633  0.433      0.294 FALSE     
       6    11  0.450  0.750      0.235 FALSE     
       7    12  0.816  0.750      0.471 FALSE     
       8    15  0.633  1.07       0.412 FALSE     
       9    18  0.816  1.38       1     FALSE     
      10    21  0.633  1.70       0.529 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1     x2      x3        x4         x5       x6         x7
        <int>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>    <dbl>      <dbl>
      1     4 -0.472  1.07   -1.77    0.00384   0.000797  -0.0288  -0.00176  
      2     5  0.491  1.51   -1.86    0.0114   -0.0106    -0.0291  -0.000367 
      3     7 -0.175  0.0562 -1.98    0.00204  -0.00179    0.0112  -0.00111  
      4     8  0.760  0.510  -1.56    0.00155   0.000204   0.0451   0.00120  
      5    11  0.626  0.785  -0.227  -0.000563  0.0126     0.0162   0.000637 
      6    12 -0.0243 1.04    0.0232  0.00725   0.000935  -0.0148   0.00153  
      7    15 -0.795  1.40    0.535   0.00194  -0.0000533  0.0119   0.00173  
      8    18  0.113  0.745   1.76   -0.00156   0.00305   -0.00738 -0.00162  
      9    21  0.628  1.63    1.61    0.00253   0.00332   -0.00610  0.0000244
      
      $df_bin_centroids
      # A tibble: 9 x 5
        hexID     c_x   c_y std_counts drop_empty
        <int>   <dbl> <dbl>      <dbl> <lgl>     
      1     4  0.0832 0.115      0.882 FALSE     
      2     5  0.450  0.115      0.176 FALSE     
      3     7 -0.1    0.433      0.118 FALSE     
      4     8  0.266  0.433      0.294 FALSE     
      5    11  0.450  0.750      0.235 FALSE     
      6    12  0.816  0.750      0.471 FALSE     
      7    15  0.633  1.07       0.412 FALSE     
      8    18  0.816  1.38       1     FALSE     
      9    21  0.633  1.70       0.529 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 10 x 8
         hb_id      x1     x2      x3        x4         x5       x6         x7
         <int>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>    <dbl>      <dbl>
       1     4 -0.472  1.07   -1.77    0.00384   0.000797  -0.0288  -0.00176  
       2     5  0.491  1.51   -1.86    0.0114   -0.0106    -0.0291  -0.000367 
       3     7 -0.175  0.0562 -1.98    0.00204  -0.00179    0.0112  -0.00111  
       4     8  0.760  0.510  -1.56    0.00155   0.000204   0.0451   0.00120  
       5     9  0.904  1.39   -0.596  -0.00257  -0.00546    0.0472   0.00125  
       6    11  0.626  0.785  -0.227  -0.000563  0.0126     0.0162   0.000637 
       7    12 -0.0243 1.04    0.0232  0.00725   0.000935  -0.0148   0.00153  
       8    15 -0.795  1.40    0.535   0.00194  -0.0000533  0.0119   0.00173  
       9    18  0.113  0.745   1.76   -0.00156   0.00305   -0.00738 -0.00162  
      10    21  0.628  1.63    1.61    0.00253   0.00332   -0.00610  0.0000244
      
      $df_bin_centroids
      # A tibble: 10 x 5
         hexID    c_x   c_y std_counts drop_empty
         <int>  <dbl> <dbl>      <dbl> <lgl>     
       1     4 0.124  0.128      0.882 FALSE     
       2     5 0.291  0.153      0.176 FALSE     
       3     7 0.0418 0.312      0.118 FALSE     
       4     8 0.302  0.335      0.294 FALSE     
       5     9 0.598  0.585      0.294 FALSE     
       6    11 0.551  0.661      0.235 FALSE     
       7    12 0.744  0.821      0.471 FALSE     
       8    15 0.783  0.995      0.412 FALSE     
       9    18 0.870  1.51       1     FALSE     
      10    21 0.789  1.68       0.529 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 9 x 8
        hb_id      x1     x2      x3        x4         x5       x6         x7
        <int>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>    <dbl>      <dbl>
      1     4 -0.472  1.07   -1.77    0.00384   0.000797  -0.0288  -0.00176  
      2     5  0.491  1.51   -1.86    0.0114   -0.0106    -0.0291  -0.000367 
      3     7 -0.175  0.0562 -1.98    0.00204  -0.00179    0.0112  -0.00111  
      4     8  0.760  0.510  -1.56    0.00155   0.000204   0.0451   0.00120  
      5    11  0.626  0.785  -0.227  -0.000563  0.0126     0.0162   0.000637 
      6    12 -0.0243 1.04    0.0232  0.00725   0.000935  -0.0148   0.00153  
      7    15 -0.795  1.40    0.535   0.00194  -0.0000533  0.0119   0.00173  
      8    18  0.113  0.745   1.76   -0.00156   0.00305   -0.00738 -0.00162  
      9    21  0.628  1.63    1.61    0.00253   0.00332   -0.00610  0.0000244
      
      $df_bin_centroids
      # A tibble: 9 x 5
        hexID    c_x   c_y std_counts drop_empty
        <int>  <dbl> <dbl>      <dbl> <lgl>     
      1     4 0.124  0.128      0.882 FALSE     
      2     5 0.291  0.153      0.176 FALSE     
      3     7 0.0418 0.312      0.118 FALSE     
      4     8 0.302  0.335      0.294 FALSE     
      5    11 0.551  0.661      0.235 FALSE     
      6    12 0.744  0.821      0.471 FALSE     
      7    15 0.783  0.995      0.412 FALSE     
      8    18 0.870  1.51       1     FALSE     
      9    21 0.789  1.68       0.529 FALSE     
      

