# Compute a benchmark value to remove long edges

This function finds the benchmark value to remove long edges based on
the differences in a distance column.

## Usage

``` r
find_lg_benchmark(distance_edges, distance_col)
```

## Arguments

- distance_edges:

  The tibble contains the distances.

- distance_col:

  The name of the column containing the distances.

## Value

The benchmark value, which is the first largest difference in the
distance column.

## Examples

``` r
distance_df <- s_curve_obj$distance_df
find_lg_benchmark(distance_edges = distance_df, distance_col = "distance")
#> Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "NULL"
```
