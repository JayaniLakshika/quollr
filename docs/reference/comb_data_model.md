# Create a tibble with averaged high-dimensional data and high-dimensional data

This function combine the average values of high-dimensional data within
each hexagonal bin and high-dimensional data.

## Usage

``` r
comb_data_model(highd_data, model_highd, model_2d)
```

## Arguments

- highd_data:

  A tibble that contains the high-dimensional data.

- model_highd:

  A tibble that contains the high-dimensional coordinates of bin
  centroids.

- model_2d:

  A tibble that contains hexagonal bin centroids in 2-D.

## Value

A tibble with the average values of the high-dimensional data within
each hexagonal bin and high-dimensional data.

## Examples

``` r
comb_data_model(highd_data = scurve,
model_highd = scurve_model_obj$model_highd,
model_2d = scurve_model_obj$model_2d)
#> # A tibble: 1,137 × 8
#>         x1     x2    x3         x4        x5       x6         x7 type 
#>      <dbl>  <dbl> <dbl>      <dbl>     <dbl>    <dbl>      <dbl> <chr>
#>  1  0.958  0.0854  1.29  0.00265    0.0171    0.0876  -0.00249   model
#>  2  0.818  0.116   1.56  0.00184    0.00361  -0.0318  -0.00377   model
#>  3  0.544  0.111   1.83 -0.00341   -0.000303  0.0196   0.0000704 model
#>  4  0.279  0.128   1.95  0.0000880  0.00104  -0.0276  -0.000227  model
#>  5  0.0567 0.119   2.00  0.00733    0.00238   0.0833  -0.00166   model
#>  6 -0.375  1.58    1.91 -0.0126     0.000689  0.0295   0.000936  model
#>  7 -0.529  1.84    1.83 -0.000870  -0.000312  0.00825  0.00162   model
#>  8  0.935  0.364   1.31 -0.00383   -0.00292  -0.00153  0.000860  model
#>  9  0.705  0.449   1.70  0.00218   -0.00679  -0.0149   0.00223   model
#> 10  0.387  0.371   1.92  0.00876    0.00420   0.0157   0.00230   model
#> # ℹ 1,127 more rows
```
