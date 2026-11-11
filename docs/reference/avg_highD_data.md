# Create a tibble with averaged high-dimensional data

This function calculates the average values of high-dimensional data
within each hexagonal bin.

## Usage

``` r
avg_highd_data(highd_data, scaled_nldr_hexid)
```

## Arguments

- highd_data:

  A tibble that contains the high-dimensional data.

- scaled_nldr_hexid:

  A tibble that contains the scaled embedding with hexagonal bin IDs.

## Value

A tibble with the average values of the high-dimensional data within
each hexagonal bin.

## Examples

``` r
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
avg_highd_data(highd_data = scurve, scaled_nldr_hexid = umap_with_hb_id)
#> # A tibble: 137 × 8
#>        h      x1     x2    x3         x4        x5       x6         x7
#>    <int>   <dbl>  <dbl> <dbl>      <dbl>     <dbl>    <dbl>      <dbl>
#>  1    34  0.958  0.0854  1.29  0.00265    0.0171    0.0876  -0.00249  
#>  2    35  0.818  0.116   1.56  0.00184    0.00361  -0.0318  -0.00377  
#>  3    36  0.544  0.111   1.83 -0.00341   -0.000303  0.0196   0.0000704
#>  4    37  0.279  0.128   1.95  0.0000880  0.00104  -0.0276  -0.000227 
#>  5    38  0.0567 0.119   2.00  0.00733    0.00238   0.0833  -0.00166  
#>  6    41 -0.375  1.58    1.91 -0.0126     0.000689  0.0295   0.000936 
#>  7    42 -0.529  1.84    1.83 -0.000870  -0.000312  0.00825  0.00162  
#>  8    49  0.935  0.364   1.31 -0.00383   -0.00292  -0.00153  0.000860 
#>  9    50  0.705  0.449   1.70  0.00218   -0.00679  -0.0149   0.00223  
#> 10    51  0.387  0.371   1.92  0.00876    0.00420   0.0157   0.00230  
#> # ℹ 127 more rows
```
