# Scaling the NLDR data

This function scales first and second columns.

## Usage

``` r
gen_scaled_data(nldr_data)
```

## Arguments

- nldr_data:

  A tibble that contains embedding components in the first and second
  columns.

## Value

A list of a tibble contains scaled first and second columns NLDR data,
and numeric vectors representing the limits of the original NLDR data.

## Examples

``` r
gen_scaled_data(nldr_data = scurve_umap)
#> $scaled_nldr
#> # A tibble: 1,000 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#>  1 0.277 0.913     1
#>  2 0.697 0.538     2
#>  3 0.779 0.399     3
#>  4 0.173 0.953     4
#>  5 0.218 0.983     5
#>  6 0.593 1.05      6
#>  7 0.180 0.210     7
#>  8 0.976 0.571     8
#>  9 0.803 0.829     9
#> 10 0.932 0.410    10
#> # ℹ 990 more rows
#> 
#> $lim1
#> [1] -9.146398  8.552211
#> 
#> $lim2
#> [1] -10.36686  10.10691
#> 
```
