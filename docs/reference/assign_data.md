# Assign data to hexagons

This function assigns the data to hexagons.

## Usage

``` r
assign_data(nldr_scaled_obj, centroids_data)
```

## Arguments

- nldr_scaled_obj:

  A list of a tibble contains scaled first and second columns of NLDR
  data, and numeric vectors representing the limits of the original NLDR
  data.

- centroids_data:

  The dataset with centroid coordinates.

## Value

A tibble contains embedding components (`emb1`, `emb2`) and
corresponding hexagon ID (`h`).

## Examples

``` r
all_centroids_df <- scurve_model_obj$hb_obj$centroids
assign_data(nldr_scaled_obj = scurve_model_obj$nldr_scaled_obj,
centroids_data = all_centroids_df)
#> # A tibble: 1,000 × 4
#>     emb1  emb2    ID     h
#>    <dbl> <dbl> <int> <int>
#>  1 0.277 0.913     1   216
#>  2 0.697 0.538     2   145
#>  3 0.779 0.399     3   116
#>  4 0.173 0.953     4   229
#>  5 0.218 0.983     5   229
#>  6 0.593 1.05      6   249
#>  7 0.180 0.210     7    79
#>  8 0.976 0.571     8   164
#>  9 0.803 0.829     9   206
#> 10 0.932 0.410    10   118
#> # ℹ 990 more rows
```
