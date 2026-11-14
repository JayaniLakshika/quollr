# Map Hexagon IDs to Centroids in the Full Grid

This function generates a data frame with hexagon IDs mapped to their
centroids in the full grid.

## Usage

``` r
map_hexbin_id(full_centroid_df, df_bin_centroids)
```

## Arguments

- full_centroid_df:

  Data frame containing centroid coordinates of the full grid.

- df_bin_centroids:

  Data frame containing hexagon IDs and their centroids.

## Value

A data frame with columns 'x', 'y', 'hexID', and 'counts' representing
hexagon centroids and counts.

## Examples

``` r
num_bins_x <- 4
shape_value <- 1.833091
hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
num_bins = num_bins_x, shape_val = shape_value)
df_bin_centroids <- hexbin_data_object$hexdf_data
full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
map_hexbin_id(full_centroid_df, df_bin_centroids)
#> # A tibble: 50 × 5
#>       c_x   c_y hexID counts std_counts
#>     <dbl> <dbl> <int>  <int>      <dbl>
#>  1 -3.27  -5.74     1      2      0.222
#>  2 -1.84  -5.74     2      6      0.667
#>  3 -0.407 -5.74     3     NA     NA    
#>  4  1.02  -5.74     4     NA     NA    
#>  5  2.46  -5.74     5     NA     NA    
#>  6 -2.55  -4.38     6      7      0.778
#>  7 -1.12  -4.38     7      5      0.556
#>  8  0.308 -4.38     8     NA     NA    
#>  9  1.74  -4.38     9     NA     NA    
#> 10  3.17  -4.38    10     NA     NA    
#> # ℹ 40 more rows
```
