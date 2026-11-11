# Calculate the effective number of bins along x-axis and y-axis

This function calculates the effective number of bins along the x and y
axes of a hexagonal grid.

## Usage

``` r
calc_bins_y(nldr_scaled_obj, b1 = 30, q = 0.1)
```

## Arguments

- nldr_scaled_obj:

  A list of a tibble contains scaled first and second columns of NLDR
  data, and numeric vectors representing the limits of the original NLDR
  data.

- b1:

  Number of bins along the x axis.

- q:

  The buffer amount as proportion of data range.

## Value

A list of numeric values that represents the effective number of bins
along the y axis, height and, width of the hexagon.

## Examples

``` r
calc_bins_y(nldr_scaled_obj = scurve_model_obj$nldr_scaled_obj, b1 = 30, q = 0.1)
#> $b2
#> [1] 40
#> 
#> $a1
#> [1] 0.04063603
#> 
#> $a2
#> [1] 0.03519184
#> 
```
