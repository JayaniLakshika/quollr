# Find points in hexagonal bins

This function maps points to their corresponding hexagonal bins.

## Usage

``` r
find_pts(scaled_nldr_hexid)
```

## Arguments

- scaled_nldr_hexid:

  A tibble that contains the scaled embedding with hexagonal bin IDs.

## Value

A tibble with hexagonal bin IDs (`h`) and the corresponding points.

## Examples

``` r
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
find_pts(scaled_nldr_hexid = umap_with_hb_id)
#> # A tibble: 137 × 2
#>        h pts_list  
#>    <int> <list>    
#>  1    34 <int [1]> 
#>  2    35 <int [10]>
#>  3    36 <int [8]> 
#>  4    37 <int [8]> 
#>  5    38 <int [2]> 
#>  6    41 <int [2]> 
#>  7    42 <int [12]>
#>  8    49 <int [11]>
#>  9    50 <int [9]> 
#> 10    51 <int [4]> 
#> # ℹ 127 more rows
```
