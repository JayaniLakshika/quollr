# Generate centroid coordinate

This function generates all possible centroids in the hexagonal grid.

## Usage

``` r
gen_centroids(nldr_scaled_obj, b1 = 30, q = 0.1)
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

A tibble contains hexIDs (`h`), x and y coordinates (`c_x`, `c_y`
respectively) of all hexagon bin centroids.

## Examples

``` r
gen_centroids(nldr_scaled_obj = scurve_model_obj$nldr_scaled_obj, b1 = 30, q = 0.1)
#> # A tibble: 1,200 × 3
#>        h     c_x    c_y
#>    <int>   <dbl>  <dbl>
#>  1     1 -0.1    -0.116
#>  2     2 -0.0594 -0.116
#>  3     3 -0.0187 -0.116
#>  4     4  0.0219 -0.116
#>  5     5  0.0625 -0.116
#>  6     6  0.103  -0.116
#>  7     7  0.144  -0.116
#>  8     8  0.184  -0.116
#>  9     9  0.225  -0.116
#> 10    10  0.266  -0.116
#> # ℹ 1,190 more rows
```
