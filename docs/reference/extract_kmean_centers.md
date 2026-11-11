# Extract k-mean center coordinates and the corresponding standardize counts.

Extract k-mean center coordinates and the corresponding standardize
counts.

## Usage

``` r
extract_kmean_centers(nldr_df, tot_bins)
```

## Arguments

- nldr_df:

  A data frame with 2D embeddings.

- tot_bins:

  Total number of bins.

## Value

A tibble contains clustering ID, kmean center coordinates, and
standardize counts.

## Examples

``` r
num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
num_bins_x <- num_bins_list$num_x
num_bins_y <- num_bins_list$num_y
extract_kmean_centers(nldr_df = s_curve_noise_umap |> dplyr::select(-ID),
tot_bins = num_bins_x * num_bins_y)
#> # A tibble: 28 × 4
#>    hexID     c_x     c_y std_counts
#>    <int>   <dbl>   <dbl>      <dbl>
#>  1     1  1.42    0.579       0.333
#>  2     2 -3.03   -3.83        0.833
#>  3     3  0.0953 -1.67        1    
#>  4     4 -2.42   -5.71        0.333
#>  5     5  1.12    1.06        0.167
#>  6     6  0.863   0.619       0.167
#>  7     7  0.662   4.13        0.333
#>  8     8 -0.590  -2.52        0.167
#>  9     9  1.17    5.81        0.167
#> 10    10  1.11   -0.0647      0.667
#> # ℹ 18 more rows
```
