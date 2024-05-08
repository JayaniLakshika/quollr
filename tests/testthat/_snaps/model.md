# fit_highd_model() works

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = TRUE,
        is_rm_lwd_hex = FALSE, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 8 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     2 -0.265  1.70  -1.88    0.00608  -0.00651   0.00410 -0.00271  
      3     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      4     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      5     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      6     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      7    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      8    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 8 x 4
        hexID    c_x    c_y std_counts
        <int>  <dbl>  <dbl>      <dbl>
      1     1 -0.1   -0.1        0.115
      2     2  0.389 -0.1        0.231
      3     4  0.145  0.324      0.577
      4     5  0.634  0.324      0.115
      5     8  0.389  0.748      0.231
      6     9  0.879  0.748      0.462
      7    11  0.634  1.17       0.154
      8    15  0.879  1.60       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = TRUE,
        is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0.25, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 7 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      3     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      4     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      5     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      6    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      7    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 7 x 4
        hexID    c_x    c_y std_counts
        <int>  <dbl>  <dbl>      <dbl>
      1     1 -0.1   -0.1        0.115
      2     4  0.145  0.324      0.577
      3     5  0.634  0.324      0.115
      4     8  0.389  0.748      0.231
      5     9  0.879  0.748      0.462
      6    11  0.634  1.17       0.154
      7    15  0.879  1.60       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = TRUE,
        is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 7 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      3     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      4     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      5     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      6    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      7    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 7 x 4
        hexID    c_x    c_y std_counts
        <int>  <dbl>  <dbl>      <dbl>
      1     1 -0.1   -0.1        0.115
      2     4  0.145  0.324      0.577
      3     5  0.634  0.324      0.115
      4     8  0.389  0.748      0.231
      5     9  0.879  0.748      0.462
      6    11  0.634  1.17       0.154
      7    15  0.879  1.60       1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = FALSE,
        is_rm_lwd_hex = FALSE, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 8 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     2 -0.265  1.70  -1.88    0.00608  -0.00651   0.00410 -0.00271  
      3     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      4     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      5     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      6     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      7    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      8    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 8 x 4
        hexID   c_x    c_y std_counts
        <int> <dbl>  <dbl>      <dbl>
      1     1 0.110 0.0366      0.115
      2     2 0.212 0.0531      0.231
      3     4 0.155 0.262       0.577
      4     5 0.503 0.468       0.115
      5     8 0.572 0.643       0.231
      6     9 0.747 0.840       0.462
      7    11 0.777 1.02        0.154
      8    15 0.842 1.57        1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = FALSE,
        is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0.25, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 7 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      3     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      4     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      5     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      6    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      7    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 7 x 4
        hexID   c_x    c_y std_counts
        <int> <dbl>  <dbl>      <dbl>
      1     1 0.110 0.0366      0.115
      2     4 0.155 0.262       0.577
      3     5 0.503 0.468       0.115
      4     8 0.572 0.643       0.231
      5     9 0.747 0.840       0.462
      6    11 0.777 1.02        0.154
      7    15 0.842 1.57        1    
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 3, s1 = -0.1, s2 = -0.1, r2 = r2, is_bin_centroid = FALSE,
        is_rm_lwd_hex = TRUE, benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 7 x 8
        hb_id      x1    x2      x3        x4        x5       x6         x7
        <int>   <dbl> <dbl>   <dbl>     <dbl>     <dbl>    <dbl>      <dbl>
      1     1 -0.909  1.46  -1.33    0.00547   0.0125   -0.0392   0.00177  
      2     4  0.0831 0.499 -1.82    0.00335  -0.000775 -0.0175  -0.000610 
      3     5  0.927  1.07  -0.926   0.00824  -0.00342   0.0608   0.00257  
      4     8  0.714  1.01  -0.335  -0.00495   0.00580   0.0234   0.000646 
      5     9 -0.127  1.11   0.0697  0.00389   0.00286   0.00384  0.0000872
      6    11 -0.841  1.66   0.647   0.00380  -0.00955  -0.00463  0.00422  
      7    15  0.291  1.05   1.71   -0.000141  0.00314  -0.00694 -0.00105  
      
      $df_bin_centroids
      # A tibble: 7 x 4
        hexID   c_x    c_y std_counts
        <int> <dbl>  <dbl>      <dbl>
      1     1 0.110 0.0366      0.115
      2     4 0.155 0.262       0.577
      3     5 0.503 0.468       0.115
      4     8 0.572 0.643       0.231
      5     9 0.747 0.840       0.462
      6    11 0.777 1.02        0.154
      7    15 0.842 1.57        1    
      

