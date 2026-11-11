# Generate Hexagonal Coordinates

This function generates the coordinates of hexagons after passing
hexagonal centroids.

## Usage

``` r
full_hex_grid(hexdf_data)
```

## Arguments

- hexdf_data:

  The dataset with all hexbin ID and centroid coordinates.

## Value

A tibble containing the coordinates of hexagons based on hexagonal
centroids.

## Examples

``` r
num_bins_x <- 4
shape_value <- 1.833091
hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
num_bins = num_bins_x, shape_val = shape_value)
df_bin_centroids <- hexbin_data_object$hexdf_data
full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
full_hex_grid(full_centroid_df)
#> # A tibble: 300 × 3
#>        x     y    id
#>    <dbl> <dbl> <int>
#>  1 -2.55 -5.29     1
#>  2 -2.55 -6.20     1
#>  3 -3.27 -6.65     1
#>  4 -3.99 -6.20     1
#>  5 -3.99 -5.29     1
#>  6 -3.27 -4.84     1
#>  7 -1.12 -5.29     2
#>  8 -1.12 -6.20     2
#>  9 -1.84 -6.65     2
#> 10 -2.55 -6.20     2
#> # ℹ 290 more rows
```
