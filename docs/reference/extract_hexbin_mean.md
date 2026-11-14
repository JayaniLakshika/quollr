# Extract hexagonal bin mean coordinates and the corresponding standardize counts.

Extract hexagonal bin mean coordinates and the corresponding standardize
counts.

## Usage

``` r
extract_hexbin_mean(data_hb, counts_data, centroids_data)
```

## Arguments

- data_hb:

  A tibble with embedding components and hexagonal bin IDs.

- counts_data:

  A tibble that contains hexagon IDs with the standardise number of
  points within each hexagon.

- centroids_data:

  A tibble that contains all hexagonal bin centroid coordinates with
  hexagon IDs.

## Value

A tibble contains hexagon ID (`h`), bin means (`c_x`, `c_y`), bin counts
(`n_h`), and standardise counts (`w_h`).

## Examples

``` r
all_centroids_df <- scurve_model_obj$hb_obj$centroids
counts_data <- scurve_model_obj$hb_obj$std_cts
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
extract_hexbin_mean(data_hb = umap_with_hb_id, counts_data = counts_data,
centroids_data = all_centroids_df)
#> # A tibble: 137 × 5
#>        h   c_x    c_y   n_h   w_h
#>    <int> <dbl>  <dbl> <dbl> <dbl>
#>  1    34 0.192 0.0440     1 0.001
#>  2    35 0.241 0.0377    10 0.01 
#>  3    36 0.325 0.0379     8 0.008
#>  4    37 0.396 0.0401     8 0.008
#>  5    38 0.451 0.0527     2 0.002
#>  6    41 0.757 0.0378     2 0.002
#>  7    42 0.808 0.0299    12 0.012
#>  8    49 0.195 0.0971    11 0.011
#>  9    50 0.269 0.101      9 0.009
#> 10    51 0.363 0.0901     4 0.004
#> # ℹ 127 more rows
```
