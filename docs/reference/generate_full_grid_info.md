# Generate Full Grid Information Data Frame

This function generates a data frame containing coordinates and
identifiers for hexagons, along with additional information like counts
and polygon IDs.

## Usage

``` r
generate_full_grid_info(df_bin_centroids)
```

## Arguments

- df_bin_centroids:

  A data frame with hexagonal bin centroids.

## Value

A data frame with columns "x", "y", "id", "c_x", "c_y", "hexID",
"counts", "std_counts", and "polygon_id".

## Examples

``` r
num_bins_x <- 4
shape_value <- 1.833091
hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
num_bins = num_bins_x, shape_val = shape_value)
df_bin_centroids <- hexbin_data_object$hexdf_data
generate_full_grid_info(df_bin_centroids)
#> # A tibble: 300 × 9
#>        x     y    id   c_x   c_y hexID counts std_counts polygon_id
#>    <dbl> <dbl> <int> <dbl> <dbl> <int>  <int>      <dbl>      <int>
#>  1 -2.55 -5.29     1 -3.27 -5.74     1      2      0.222          1
#>  2 -2.55 -6.20     1 -3.27 -5.74     1      2      0.222          1
#>  3 -3.27 -6.65     1 -3.27 -5.74     1      2      0.222          1
#>  4 -3.99 -6.20     1 -3.27 -5.74     1      2      0.222          1
#>  5 -3.99 -5.29     1 -3.27 -5.74     1      2      0.222          1
#>  6 -3.27 -4.84     1 -3.27 -5.74     1      2      0.222          1
#>  7 -1.12 -5.29     2 -1.84 -5.74     2      6      0.667          2
#>  8 -1.12 -6.20     2 -1.84 -5.74     2      6      0.667          2
#>  9 -1.84 -6.65     2 -1.84 -5.74     2      6      0.667          2
#> 10 -2.55 -6.20     2 -1.84 -5.74     2      6      0.667          2
#> # ℹ 290 more rows
```
