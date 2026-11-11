# Predict 2D Embeddings

Given a test dataset, the centroid coordinates of hexagonal bins in 2D
and high-dimensional space, predict the 2D embeddings for each data
point in the test dataset.

## Usage

``` r
predict_2d_embeddings(test_data, df_bin_centroids, df_bin, type_NLDR = "UMAP")
```

## Arguments

- test_data:

  The test dataset containing high-dimensional coordinates and an unique
  identifier.

- df_bin_centroids:

  Centroid coordinates of hexagonal bins in 2D space.

- df_bin:

  Centroid coordinates of hexagonal bins in high dimensions.

- type_NLDR:

  The type of non-linear dimensionality reduction (NLDR) used. Default
  is "UMAP".

## Value

A list contains predicted 2D embeddings, ID in the test data, and
predicted hexagonal IDs.

## Examples

``` r
training_data <- s_curve_noise_training
num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", hex_size = NA, buffer_x = NA)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
 y = "UMAP2", hex_size = NA, buffer_y = NA)
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> Hex size is set to 0.2.
all_centroids_df <- as.data.frame(do.call(cbind, hex_bin_obj$full_grid_hex_centroids))
counts_df <- as.data.frame(do.call(cbind, hex_bin_obj$hex_id_with_std_counts))
df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
df_bin <- avg_highD_data(df_all, column_start_text = "x")
predict_2d_embeddings(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
df_bin = df_bin, type_NLDR = "UMAP")
#> $pred_UMAP_1
#>  [1] 0.1732051 0.6928203 0.8660254 0.1732051 0.1732051 0.8660254 0.6928203
#>  [8] 0.6928203 0.6928203 0.8660254 0.6928203 0.1732051 1.0392305 0.0000000
#> [15] 0.6928203 0.6928203 0.1732051 0.6928203 0.5196152 0.1732051 0.6928203
#> [22] 0.1732051 0.0000000 0.8660254 0.8660254 0.3464102 0.5196152 0.6928203
#> [29] 0.3464102 0.6928203 0.8660254 0.8660254 0.6928203 0.1732051 0.8660254
#> [36] 0.1732051 0.6928203 0.8660254 0.8660254 0.0000000 0.5196152 0.8660254
#> [43] 0.3464102 0.8660254 0.8660254 0.6928203 0.1732051 0.0000000 0.8660254
#> [50] 0.0000000 0.6928203 0.6928203 0.8660254 0.3464102 0.6928203 0.8660254
#> [57] 0.8660254 1.0392305 1.0392305 0.5196152 0.8660254 1.0392305 0.5196152
#> [64] 0.8660254 0.3464102 0.3464102 1.0392305 0.8660254 0.6928203 1.0392305
#> [71] 0.8660254 0.5196152 0.1732051 0.3464102 1.0392305
#> 
#> $pred_UMAP_2
#>  [1]  0.45  0.75  1.05 -0.15  0.45  1.65  0.75  0.75  0.75  1.65  0.75  0.45
#> [13]  1.35  0.15  0.75  1.35  0.45  1.35  0.45 -0.15  0.75 -0.15  0.15  1.65
#> [25]  1.05  0.15  0.45  0.75  0.15  0.75  1.05  1.65  0.75  0.45  1.65 -0.15
#> [37]  1.35  1.65  1.65  0.15  0.45  1.05  0.15  1.05  1.05  1.35 -0.15  0.15
#> [49]  1.65  0.15  0.75  0.75  1.65  0.15  1.35  1.65  1.05  1.35  1.35  0.45
#> [61]  1.65  1.35  0.45  1.65  0.15  0.15  1.35  1.05  1.35  1.35  1.65  0.45
#> [73] -0.15  0.15  1.35
#> 
#> $ID
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pred_hb_id
#>  [1] 12 18 24  2 12 34 18 18 18 34 18 12 29  6 18 28 12 28 13  2 18  2  6 34 24
#> [26]  7 13 18  7 18 24 34 18 12 34  2 28 34 34  6 13 24  7 24 24 28  2  6 34  6
#> [51] 18 18 34  7 28 34 24 29 29 13 34 29 13 34  7  7 29 24 28 29 34 13  2  7 29
#> 
```
