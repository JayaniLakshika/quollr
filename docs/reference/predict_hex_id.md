# Predict Hexagonal IDs

This function predicts hexagonal IDs for a test set based on existing
bin centroids.

## Usage

``` r
predict_hex_id(df_bin_centroids, nldr_df_test, x = "UMAP1", y = "UMAP2")
```

## Arguments

- df_bin_centroids:

  The training dataset containing high-dimensional data with IDs.

- nldr_df_test:

  The non-linear dimensionality reductions that need to find the
  prediction.

- x:

  The name of the column that contains first 2D embeddings component.

- y:

  The name of the column that contains second 2D embeddings component.

## Value

A data frame containing prediced hexID for 2D embedding data

## Examples

``` r
num_bins_x <- 4
shape_value <- 1.833091
hexbin_data_object <- extract_hexbin_mean(nldr_df = s_curve_noise_umap, num_bins_x,
shape_val = shape_value)
df_bin_centroids <- hexbin_data_object$hexdf_data
predict_hex_id(df_bin_centroids = df_bin_centroids, nldr_df_test = s_curve_noise_umap,
x = "UMAP1", y = "UMAP2")
#> # A tibble: 75 × 4
#>     UMAP1    UMAP2    ID pred_hb_id
#>     <dbl>    <dbl> <int>      <dbl>
#>  1 -2.81  -3.91        1          6
#>  2  0.959 -0.00271     2         24
#>  3  1.54   0.462       3         29
#>  4 -2.31  -5.50        4          2
#>  5 -1.76  -3.46        6         12
#>  6  1.53   5.75        7         44
#>  7  0.930 -0.175       8         24
#>  8  0.319 -1.61        9         18
#>  9  1.37   0.0541     11         24
#> 10  1.90   4.94       12         45
#> # ℹ 65 more rows
```
