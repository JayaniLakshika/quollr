# find_benchmark_value() works

    Code
      find_benchmark_value(data_dist, "dist")
    Condition
      Warning in `find_benchmark_value()`:
      Difference between unique distances are increasing. Please use a suitable value for benchmark.
    Output
      [1] NA

# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = num_bins_x)
    Output
      # A tibble: 16 x 6
              x      y hexID counts std_counts mean_density
          <dbl>  <dbl> <int>  <int>      <dbl>        <dbl>
       1 -3.27  -5.74      1      2      0.222        0.722
       2 -1.84  -5.74      2      6      0.667        0.519
       3 -2.55  -4.38      6      7      0.778        0.417
       4 -1.12  -4.38      7      5      0.556        0.472
       5 -3.27  -3.01     11      2      0.222        0.519
       6 -1.84  -3.01     12      2      0.222        0.278
       7 -0.407 -3.01     13      2      0.222        0.407
       8 -1.12  -1.65     17      1      0.111        0.444
       9  0.308 -1.65     18      8      0.889        0.167
      10  1.02  -0.280    24      7      0.778        0.389
      11  0.308  1.09     28      1      0.111        0.722
      12  1.74   1.09     29      6      0.667        0.444
      13  0.308  3.82     38      2      0.222        0.889
      14  1.74   3.82     39      8      0.889        0.611
      15  1.02   5.18     44      9      1            0.833
      16  2.46   5.18     45      7      0.778        1    

---

    Code
      compute_mean_density_hex(df_bin_centroids = df_bin_centroids, num_bins_x = NA)
    Condition
      Error in `compute_mean_density_hex()`:
      ! Number of bins along x axis is not defined.

# find_low_density_hexagons() works

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts")
    Output
      [1] 12 13

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = data.frame(matrix(nrow = 0, ncol = 0)),
        col_std_counts = "std_counts")
    Output
      [1] 12 13

---

    Code
      find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
        df_bin_centroids_low = df_bin_centroids_low, col_std_counts = "std_counts")
    Message
      Don't need to remove low-density hexagonal bins.
    Output
      NULL

# extract_coord_of_shifted_hex_grid() works

    Code
      extract_coord_of_shifted_hex_grid(nldr_data_with_hb_id = UMAP_data_with_hb_id,
        num_bins_x = num_bins_x, hex_full_count_df = hex_full_count_df, shift_x = NA,
        shift_y = NA, cell_area = 1)
    Output
      $hex_full_count_df_new
      # A tibble: 300 x 9
             x     y    id   c_x   c_y hexID polygon_id counts std_counts
         <dbl> <dbl> <int> <dbl> <dbl> <int>      <int>  <int>      <dbl>
       1 -3.09 -5.83     1 -3.81 -6.28     1          1     NA       NA  
       2 -3.09 -6.73     1 -3.81 -6.28     1          1     NA       NA  
       3 -3.81 -7.19     1 -3.81 -6.28     1          1     NA       NA  
       4 -4.52 -6.73     1 -3.81 -6.28     1          1     NA       NA  
       5 -4.52 -5.83     1 -3.81 -6.28     1          1     NA       NA  
       6 -3.81 -5.37     1 -3.81 -6.28     1          1     NA       NA  
       7 -1.66 -5.83     2 -2.38 -6.28     2          2      4        0.4
       8 -1.66 -6.73     2 -2.38 -6.28     2          2      4        0.4
       9 -2.38 -7.19     2 -2.38 -6.28     2          2      4        0.4
      10 -3.09 -6.73     2 -2.38 -6.28     2          2      4        0.4
      # i 290 more rows
      
      $nldr_df_with_new_hexID
              UMAP1        UMAP2 hb_id
      1  -2.8097466 -3.910005483     6
      2   0.9587085 -0.002708352    29
      3   1.5389855  0.462163394    29
      4  -2.3129820 -5.495084256     2
      5  -1.7648455 -3.461140117    12
      6   1.5291731  5.746500231    49
      7   0.9304771 -0.175236058    29
      8   0.3194170 -1.614071523    18
      9   1.3675692  0.054122172    29
      10  1.9025902  4.943403570    45
      11  0.4080949 -1.525707795    18
      12 -2.9735853 -3.600461658    11
      13  2.2396519  4.583359135    45
      14 -2.8567553 -4.284969555     6
      15  0.4467211 -1.780856063    18
      16  1.3552697  3.435359060    39
      17 -2.4962950 -3.502364361    12
      18  1.3486792  3.477064406    39
      19  0.1830594 -1.754456198    18
      20 -2.3455605 -5.676414556     2
      21  0.8137371 -0.419929238    24
      22 -2.7317882 -5.350256398     6
      23 -2.7607269 -4.058474305     6
      24  1.1678508  5.807591478    49
      25  1.2701112  1.606845833    34
      26 -1.6341561 -4.391463483     7
      27 -0.5902292 -2.519974956    18
      28  0.0244208 -1.012724342    23
      29 -2.0648469 -4.728244274     6
      30  0.3033282 -0.986658735    23
      31  1.1236335  1.058988539    29
      32  1.1058204  4.622732304    44
      33  0.4424786 -1.323557255    18
      34 -3.0888293 -3.715523544    11
      35  2.2523472  5.375278885    50
      36 -2.6910127 -5.402672793     6
      37  0.8102587  4.536541146    44
      38  1.4767116  5.758544173    49
      39  1.3834120  5.820530794    49
      40 -3.2703577 -3.928162328    11
      41 -1.0720147 -3.137653586    13
      42  1.4614730  1.011057982    29
      43 -1.5951242 -4.821159579     7
      44  1.5670651  0.820999232    29
      45  1.3101234  0.696316310    29
      46  0.6999723  3.925631251    38
      47 -2.4996853 -5.742540480     2
      48 -3.0293226 -4.020706179     6
      49  2.1260682  5.479212510    50
      50 -2.9640458 -4.487579628     6
      51  0.6620562 -0.544895479    24
      52  1.1860855 -0.135167840    29
      53  1.0530198  5.412247159    49
      54 -1.9462124 -5.335024462     7
      55  1.1289559  4.252394278    44
      56  0.7288703  5.083452963    44
      57  0.8913631  0.477821407    29
      58  2.4165932  4.412252828    45
      59  2.2395888  4.579307233    45
      60 -1.0191663 -3.551255942    12
      61  0.9769054  5.485903919    49
      62  2.1492828  4.432353405    39
      63 -0.3539761 -1.620782255    18
      64  2.4555949  5.109462560    45
      65 -1.3494107 -3.882030702     7
      66 -1.8130766 -5.321458199     7
      67  1.8021561  3.660608734    39
      68  0.8633422  0.619339211    28
      69  0.6249804  4.329653948    38
      70  2.0282706  3.973496871    39
      71  1.3656407  4.467208531    44
      72 -0.4312690 -1.703695332    17
      73 -2.3369666 -5.540483168     2
      74 -1.5812238 -4.959468236     7
      75  1.9432959  3.907273242    39
      

