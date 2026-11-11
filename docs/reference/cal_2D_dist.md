# Calculate 2D Euclidean distances between vertices

This function calculates the 2D distances between pairs of points in a
data frame.

## Usage

``` r
cal_2d_dist(tr_coord_df, start_x, start_y, end_x, end_y, select_vars)
```

## Arguments

- tr_coord_df:

  A tibble that contains the x and y coordinates of start and end
  points.

- start_x:

  Column name for the x-coordinate of the starting point.

- start_y:

  Column name for the y-coordinate of the starting point.

- end_x:

  Column name for the x-coordinate of the ending point.

- end_y:

  Column name for the y-coordinate of the ending point.

- select_vars:

  A character vector specifying the columns to be selected in the
  resulting data frame.

## Value

A tibble with columns for the starting point, ending point, and
calculated distances.

## Examples

``` r
tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from", start_y = "y_from",
end_x = "x_to", end_y = "y_to", select_vars = c("from", "to", "distance"))
#> # A tibble: 35 × 3
#>     from    to distance
#>    <int> <int>    <dbl>
#>  1     1     2    0.327
#>  2     4     5    0.327
#>  3     3     5    0.327
#>  4     5     6    0.327
#>  5     5    11    0.566
#>  6     5     8    0.327
#>  7     1    10    0.865
#>  8     5    10    0.654
#>  9    10    11    0.327
#> 10     8     9    0.327
#> # ℹ 25 more rows
```
