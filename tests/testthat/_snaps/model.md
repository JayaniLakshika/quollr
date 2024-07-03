# fit_highd_model() works

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 4, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id       x1    x2     x3       x4        x5        x6        x7
        <int>    <dbl> <dbl>  <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
      1     2 -0.588   1.62  -1.67   0.00632  0.00197  -0.00708  -0.00159 
      2     6  0.376   0.817 -1.28   0.00191 -0.000152  0.000154 -0.000256
      3    11 -0.487   1.10   0.684  0.00339 -0.000653  0.00762   0.00173 
      4    14 -0.00224 1.69   1.90   0.00336  0.00915  -0.0324    0.000345
      5    15  0.617   0.945  1.64  -0.00138  0.00314  -0.00876  -0.00192 
      
      $df_bin_centroids
      # A tibble: 5 x 5
        hexID    c_x    c_y std_counts drop_empty
        <int>  <dbl>  <dbl>      <dbl> <lgl>     
      1     2 -0.1   -0.202      0.296 FALSE     
      2     6  0.328  0.539      1     FALSE     
      3    11  0.755  1.28       0.704 FALSE     
      4    14  0.328  2.02       0.148 FALSE     
      5    15  1.18   2.02       0.630 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 4, r2 = r2, is_bin_centroid = TRUE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id       x1    x2     x3       x4        x5        x6        x7
        <int>    <dbl> <dbl>  <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
      1     2 -0.588   1.62  -1.67   0.00632  0.00197  -0.00708  -0.00159 
      2     6  0.376   0.817 -1.28   0.00191 -0.000152  0.000154 -0.000256
      3    11 -0.487   1.10   0.684  0.00339 -0.000653  0.00762   0.00173 
      4    14 -0.00224 1.69   1.90   0.00336  0.00915  -0.0324    0.000345
      5    15  0.617   0.945  1.64  -0.00138  0.00314  -0.00876  -0.00192 
      
      $df_bin_centroids
      # A tibble: 5 x 5
        hexID    c_x    c_y std_counts drop_empty
        <int>  <dbl>  <dbl>      <dbl> <lgl>     
      1     2 -0.1   -0.202      0.296 FALSE     
      2     6  0.328  0.539      1     FALSE     
      3    11  0.755  1.28       0.704 FALSE     
      4    14  0.328  2.02       0.148 FALSE     
      5    15  1.18   2.02       0.630 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 4, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = FALSE,
        col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id       x1    x2     x3       x4        x5        x6        x7
        <int>    <dbl> <dbl>  <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
      1     2 -0.588   1.62  -1.67   0.00632  0.00197  -0.00708  -0.00159 
      2     6  0.376   0.817 -1.28   0.00191 -0.000152  0.000154 -0.000256
      3    11 -0.487   1.10   0.684  0.00339 -0.000653  0.00762   0.00173 
      4    14 -0.00224 1.69   1.90   0.00336  0.00915  -0.0324    0.000345
      5    15  0.617   0.945  1.64  -0.00138  0.00314  -0.00876  -0.00192 
      
      $df_bin_centroids
      # A tibble: 5 x 5
        hexID   c_x    c_y std_counts drop_empty
        <int> <dbl>  <dbl>      <dbl> <lgl>     
      1     2 0.163 0.0453      0.296 FALSE     
      2     6 0.328 0.458       1     FALSE     
      3    11 0.776 1.23        0.704 FALSE     
      4    14 0.708 1.85        0.148 FALSE     
      5    15 0.888 1.87        0.630 FALSE     
      

---

    Code
      fit_highd_model(training_data = s_curve_noise_training, emb_df = s_curve_noise_umap_scaled,
        bin1 = 4, r2 = r2, is_bin_centroid = FALSE, is_rm_lwd_hex = TRUE,
        benchmark_to_rm_lwd_hex = 0.4, col_start_highd = "x")
    Output
      $df_bin
      # A tibble: 5 x 8
        hb_id       x1    x2     x3       x4        x5        x6        x7
        <int>    <dbl> <dbl>  <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
      1     2 -0.588   1.62  -1.67   0.00632  0.00197  -0.00708  -0.00159 
      2     6  0.376   0.817 -1.28   0.00191 -0.000152  0.000154 -0.000256
      3    11 -0.487   1.10   0.684  0.00339 -0.000653  0.00762   0.00173 
      4    14 -0.00224 1.69   1.90   0.00336  0.00915  -0.0324    0.000345
      5    15  0.617   0.945  1.64  -0.00138  0.00314  -0.00876  -0.00192 
      
      $df_bin_centroids
      # A tibble: 5 x 5
        hexID   c_x    c_y std_counts drop_empty
        <int> <dbl>  <dbl>      <dbl> <lgl>     
      1     2 0.163 0.0453      0.296 FALSE     
      2     6 0.328 0.458       1     FALSE     
      3    11 0.776 1.23        0.704 FALSE     
      4    14 0.708 1.85        0.148 FALSE     
      5    15 0.888 1.87        0.630 FALSE     
      

