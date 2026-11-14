# Compute Projection for High-Dimensional Data

Compute Projection for High-Dimensional Data

## Usage

``` r
get_projection(projection, highd_data, model_highd, trimesh_data, axis_param)
```

## Arguments

- projection:

  A matrix or data frame representing the projection.

- highd_data:

  A data frame or matrix of high-dimensional data.

- model_highd:

  A model object or function used for high-dimensional transformation.

- trimesh_data:

  A data frame defining transformation from one space to another.

- axis_param:

  A list of parameters for axis configuration.

## Value

A data frame or matrix with the transformed projection.

## Examples

``` r
projection_df <- cbind(
c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

df_bin <- scurve_model_obj$model_highd
edge_data <- scurve_model_obj$trimesh_data

get_projection(projection = projection_df,
highd_data = scurve, model_highd = df_bin,
trimesh_data = edge_data,
axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
axis_pos_y = -0.72,threshold = 0.09))
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> $projected_df
#> # A tibble: 1,000 × 3
#>       proj1  proj2    ID
#>       <dbl>  <dbl> <int>
#>  1 -0.400   0.182      1
#>  2  0.00561 0.0247     2
#>  3  0.187   0.174      3
#>  4 -0.276   0.231      4
#>  5 -0.320   0.243      5
#>  6 -0.509   0.221      6
#>  7  0.0934  0.0246     7
#>  8 -0.0438  0.294      8
#>  9 -0.284   0.116      9
#> 10  0.147   0.289     10
#> # ℹ 990 more rows
#> 
#> $model_df
#> # A tibble: 346 × 14
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
#> # ℹ 5 more variables: to_reindexed <int>, proj1_from <dbl>, proj2_from <dbl>,
#> #   proj1_to <dbl>, proj2_to <dbl>
#> 
#> $axes
#>       x1    y1        x2        y2   distance
#> x1 -0.72 -0.72 -0.806765 -0.772755 0.10154435
#> x3 -0.72 -0.72 -0.620715 -0.730315 0.09981939
#> x6 -0.72 -0.72 -0.745095 -0.621510 0.10163680
#> 
#> $circle
#>            c1         c2
#> 1  -0.5533333 -0.7200000
#> 2  -0.5547017 -0.6986871
#> 3  -0.5587842 -0.6777242
#> 4  -0.5655139 -0.6574555
#> 5  -0.5747802 -0.6382137
#> 6  -0.5864311 -0.6203149
#> 7  -0.6002751 -0.6040529
#> 8  -0.6160850 -0.5896948
#> 9  -0.6336012 -0.5774762
#> 10 -0.6525361 -0.5675979
#> 11 -0.6725787 -0.5602220
#> 12 -0.6934000 -0.5554697
#> 13 -0.7146581 -0.5534190
#> 14 -0.7360038 -0.5541035
#> 15 -0.7570868 -0.5575120
#> 16 -0.7775608 -0.5635886
#> 17 -0.7970897 -0.5722334
#> 18 -0.8153528 -0.5833046
#> 19 -0.8320501 -0.5966203
#> 20 -0.8469077 -0.6119619
#> 21 -0.8596814 -0.6290775
#> 22 -0.8701615 -0.6476860
#> 23 -0.8781760 -0.6674820
#> 24 -0.8835932 -0.6881402
#> 25 -0.8863242 -0.7093216
#> 26 -0.8863242 -0.7306784
#> 27 -0.8835932 -0.7518598
#> 28 -0.8781760 -0.7725180
#> 29 -0.8701615 -0.7923140
#> 30 -0.8596814 -0.8109225
#> 31 -0.8469077 -0.8280381
#> 32 -0.8320501 -0.8433797
#> 33 -0.8153528 -0.8566954
#> 34 -0.7970897 -0.8677666
#> 35 -0.7775608 -0.8764114
#> 36 -0.7570868 -0.8824880
#> 37 -0.7360038 -0.8858965
#> 38 -0.7146581 -0.8865810
#> 39 -0.6934000 -0.8845303
#> 40 -0.6725787 -0.8797780
#> 41 -0.6525361 -0.8724021
#> 42 -0.6336012 -0.8625238
#> 43 -0.6160850 -0.8503052
#> 44 -0.6002751 -0.8359471
#> 45 -0.5864311 -0.8196851
#> 46 -0.5747802 -0.8017863
#> 47 -0.5655139 -0.7825445
#> 48 -0.5587842 -0.7622758
#> 49 -0.5547017 -0.7413129
#> 50 -0.5533333 -0.7200000
#> 
```
