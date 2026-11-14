# Calculate 2-D Euclidean distances between vertices

This function calculates the 2-D distances between pairs of points in a
data frame.

## Usage

``` r
calc_2d_dist(
  trimesh_data,
  select_vars = c("from", "to", "x_from", "y_from", "x_to", "y_to", "from_count",
    "to_count", "distance")
)
```

## Arguments

- trimesh_data:

  A tibble that contains the x and y coordinates of start and end
  points.

- select_vars:

  selected columns in the resulting data frame.

## Value

A tibble with columns for the starting point, ending point, and
calculated distances.

## Examples

``` r
tr_from_to_df <- scurve_model_obj$trimesh_data
calc_2d_dist(trimesh_data = tr_from_to_df)
#> # A tibble: 346 × 9
#>     from    to x_from y_from  x_to   y_to from_count to_count distance
#>    <int> <int>  <dbl>  <dbl> <dbl>  <dbl>      <dbl>    <dbl>    <dbl>
#>  1    35    49  0.234 0.0288 0.192 0.101          10       11   0.0834
#>  2    34    35  0.150 0.0288 0.234 0.0288          1       10   0.0834
#>  3    49    65  0.192 0.101  0.234 0.173          11       10   0.0834
#>  4    35    36  0.234 0.0288 0.317 0.0288         10        8   0.0834
#>  5    64    65  0.150 0.173  0.234 0.173           5       10   0.0834
#>  6    35    50  0.234 0.0288 0.275 0.101          10        9   0.0834
#>  7    64    79  0.150 0.173  0.192 0.245           5       10   0.0834
#>  8    36    50  0.317 0.0288 0.275 0.101           8        9   0.0834
#>  9    78    94  0.109 0.245  0.150 0.318           1       11   0.0834
#> 10    78    79  0.109 0.245  0.192 0.245           1       10   0.0834
#> # ℹ 336 more rows
```
