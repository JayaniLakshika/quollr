# Generate erros and MSE for different bin widths

This function augments a dataset with predictions and error metrics
obtained from a nonlinear dimension reduction (NLDR) model.

## Usage

``` r
gen_diffbin1_errors(highd_data, nldr_data, hd_thresh = 1, bin1_vec = NULL)
```

## Arguments

- highd_data:

  A tibble that contains the high-dimensional data with a unique
  identifier.

- nldr_data:

  A tibble that contains the embedding with a unique identifier.

- hd_thresh:

  (default: 1) A numeric value using to filter high-density hexagons.

- bin1_vec:

  A numeric vector contains the range of b1 values.

## Value

A tibble containing the augmented data with predictions, error metrics,
and absolute error metrics.

## Examples

``` r
scurve_sample <- scurve |> head(100)
scurve_umap_sample <- scurve_umap |> head(100)
gen_diffbin1_errors(highd_data = scurve_sample, nldr_data = scurve_umap_sample)
#> ✔ Model generated successfully!!!
#> ✔ Model generated successfully!!!
#> ✔ Model generated successfully!!!
#> ✔ Model generated successfully!!!
#> ✔ Model generated successfully!!!
#>      Error       HBE b1 b2   b  m   a1   a2    n_bar
#> 1 56.41496 0.3864918  5  7  35 18 0.26 0.23 5.555556
#> 2 48.44832 0.3431850  6  8  48 21 0.22 0.19 4.761905
#> 3 45.81080 0.3174797  7  9  63 25 0.20 0.17 4.000000
#> 4 41.79401 0.2989940  8 11  88 28 0.16 0.14 3.571429
#> 5 42.09304 0.3126706  9 12 108 29 0.14 0.12 3.448276
```
