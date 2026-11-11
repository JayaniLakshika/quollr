# Compute standardise counts in hexagons

This function computes the standardize number of points within each
hexagon.

## Usage

``` r
compute_std_counts(scaled_nldr_hexid)
```

## Arguments

- scaled_nldr_hexid:

  A tibble that contains the scaled embedding with hexagonal bin IDs.

## Value

A tibble that contains hexagon IDs (`h`), bin counts (`n_h`), and
standardize counts (`w_h`).

## Examples

``` r
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
compute_std_counts(scaled_nldr_hexid = umap_with_hb_id)
#> # A tibble: 137 × 3
#>        h   n_h   w_h
#>    <int> <int> <dbl>
#>  1    34     1 0.001
#>  2    35    10 0.01 
#>  3    36     8 0.008
#>  4    37     8 0.008
#>  5    38     2 0.002
#>  6    41     2 0.002
#>  7    42    12 0.012
#>  8    49    11 0.011
#>  9    50     9 0.009
#> 10    51     4 0.004
#> # ℹ 127 more rows
```
