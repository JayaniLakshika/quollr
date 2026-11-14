# Update from and to values in trimesh data

This function update the from and to indexes.

## Usage

``` r
update_trimesh_index(trimesh_data)
```

## Arguments

- trimesh_data:

  A tibble that contains wireframe data.

## Value

A tibble that contains the updated edge information.

## Examples

``` r
tr_from_to_df <- scurve_model_obj$trimesh_data
update_trimesh_index(trimesh_data = tr_from_to_df)
#> # A tibble: 346 × 10
#>     from    to x_from y_from  x_to   y_to from_count to_count from_reindexed
#>    <int> <int>  <dbl>  <dbl> <dbl>  <dbl>      <dbl>    <dbl>          <int>
#>  1    35    49  0.234 0.0288 0.192 0.101          10       11              2
#>  2    34    35  0.150 0.0288 0.234 0.0288          1       10              1
#>  3    49    65  0.192 0.101  0.234 0.173          11       10              8
#>  4    35    36  0.234 0.0288 0.317 0.0288         10        8              2
#>  5    64    65  0.150 0.173  0.234 0.173           5       10             17
#>  6    35    50  0.234 0.0288 0.275 0.101          10        9              2
#>  7    64    79  0.150 0.173  0.192 0.245           5       10             17
#>  8    36    50  0.317 0.0288 0.275 0.101           8        9              3
#>  9    78    94  0.109 0.245  0.150 0.318           1       11             27
#> 10    78    79  0.109 0.245  0.192 0.245           1       10             27
#> # ℹ 336 more rows
#> # ℹ 1 more variable: to_reindexed <int>
```
