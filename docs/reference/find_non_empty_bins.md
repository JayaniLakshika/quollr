# Find the number of bins required to achieve required number of non-empty bins.

This function determines the number of bins along the x and y axes to
obtain a specific number of non-empty bins.

## Usage

``` r
find_non_empty_bins(nldr_scaled_obj, m = 2, q = 0.1)
```

## Arguments

- nldr_scaled_obj:

  A list of a tibble contains scaled first and second columns of NLDR
  data, and numeric vectors representing the limits of the original NLDR
  data.

- m:

  The desired number of non-empty bins.

- q:

  The buffer amount as proportion of data range.

## Value

The number of bins along the x and y axes needed to achieve a specific
number of non-empty bins.

## Examples

``` r
find_non_empty_bins(nldr_scaled_obj = scurve_model_obj$nldr_scaled_obj, m = 5)
```
