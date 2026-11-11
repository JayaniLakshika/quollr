# Compute mean density of hexagonal bins

This function calculates the mean density of hexagonal bins based on
their neighboring bins.

## Usage

``` r
compute_mean_density_hex(model_2d, b1 = 30)
```

## Arguments

- model_2d:

  A tibble that contains information about hexagonal bin centroids,
  including the hexagon ID and the standardised counts (`w_h`).

- b1:

  The number of bins along the x-axis for the hexagonal grid.

## Value

A tibble contains hexagonal IDs and the mean density of each hexagonal
bin based on its neighboring bins.

## Examples

``` r
compute_mean_density_hex(model_2d = scurve_model_obj$model_2d, b1 = 30)
#> # A tibble: 137 × 2
#>    hb_id mean_density
#>    <int>        <dbl>
#>  1    34      0.00833
#>  2    35      0.00725
#>  3    36      0.00975
#>  4    37      0.0065 
#>  5    38      0.009  
#>  6    41      0.009  
#>  7    42      0.00467
#>  8    49      0.008  
#>  9    50      0.007  
#> 10    51      0.009  
#> # ℹ 127 more rows
```
