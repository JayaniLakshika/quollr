# Augment Data with Predictions and Error Metrics for NLDR Models

This S3 method augments a dataset with predictions and error metrics
obtained from a nonlinear dimension reduction (NLDR) model stored in a
`highd_vis_model` object.

## Usage

``` r
# S3 method for class 'highd_vis_model'
augment(x, highd_data, ...)
```

## Arguments

- x:

  An object of class `highd_vis_model` containing the model outputs.

- highd_data:

  A data frame or tibble containing the original high-dimensional
  coordinates with an `ID` column.

- ...:

  Additional arguments (currently unused).

## Value

A tibble containing the augmented data with predictions, error metrics,
and absolute error metrics.

## Examples

``` r
# Assuming 'fit' is a highd_vis_model object and 'scurve' contains the original data:
fit <- fit_highd_model(highd_data = scurve, nldr_data = scurve_umap, b1 = 30,
q = 0.1, hd_thresh = 5)
#> ✔ Model generated successfully!!!
augment(x = fit, highd_data = scurve)
#> # A tibble: 1,000 × 32
#>       ID      x1    x2       x3       x4       x5       x6        x7 pred_h
#>    <int>   <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>     <dbl>  <int>
#>  1     1 -0.120  0.819 -1.99     0.0114   0.00351  0.0334   0.00638     816
#>  2     2 -0.0492 0.166  0.00121  0.0115  -0.0166  -0.0297   0.00509     711
#>  3     3 -0.774  0.651  0.367   -0.0172   0.00600  0.0211   0.00303     287
#>  4     4 -0.606  0.952 -1.80     0.0157  -0.00978 -0.0590  -0.00754     816
#>  5     5 -0.478  1.10  -1.88    -0.00423  0.00495 -0.0482  -0.00982     816
#>  6     6  0.818  1.78  -1.58     0.0124   0.0198   0.0560  -0.000730    711
#>  7     7  0.910  0.975  1.42    -0.0111   0.0132   0.0299   0.00401     397
#>  8     8 -0.0691 1.90   0.00239  0.0125  -0.00463  0.0260   0.00590     711
#>  9     9  0.859  1.34  -0.488   -0.00195 -0.0145  -0.00950  0.00593     711
#> 10    10 -0.727  1.56   0.314    0.0189   0.0147  -0.0659   0.00617     711
#> # ℹ 990 more rows
#> # ℹ 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
#> #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
#> #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
#> #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
#> #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
#> #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>, …

```
