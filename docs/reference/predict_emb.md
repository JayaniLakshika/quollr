# Predict 2-D embeddings

Given a test dataset, the centroid coordinates of hexagonal bins in 2-D
and high-dimensional space, predict the 2-D embeddings for each data
point in the test dataset.

## Usage

``` r
predict_emb(highd_data, model_2d, model_highd)
```

## Arguments

- highd_data:

  The tibble contains high-dimensional data and an unique identifier.

- model_2d:

  Centroid coordinates of hexagonal bins in 2-D space.

- model_highd:

  Centroid coordinates of hexagonal bins in high dimensions.

## Value

A tibble contains predicted 2-D embeddings, ID in the test data, and
predicted hexagonal IDs.

## Examples

``` r
predict_emb(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
model_2d = scurve_model_obj$model_2d)
#> # A tibble: 1,000 × 4
#>    pred_emb_1 pred_emb_2    ID pred_h
#>         <dbl>      <dbl> <int>  <int>
#>  1      0.234      0.896     1    215
#>  2      0.692      0.534     2    145
#>  3      0.776      0.390     3    116
#>  4      0.192      0.968     4    229
#>  5      0.192      0.968     5    229
#>  6      0.567      1.04      6    249
#>  7      0.192      0.245     7     79
#>  8      0.984      0.607     8    164
#>  9      0.776      0.823     9    206
#> 10      0.943      0.390    10    118
#> # ℹ 990 more rows
```
