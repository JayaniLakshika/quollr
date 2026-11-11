# Calculate effective shape parameter value

This function calculates the effective shape parameter value for a
scatter plot based on the ranges of the x-axis and y-axis.

## Usage

``` r
calculate_effective_shape_value(.data, x = "UMAP1", y = "UMAP2")
```

## Arguments

- .data:

  A data frame, data frame extension (e.g., a tibble), or a lazy data
  frame (e.g., from dbplyr or dtplyr).

- x:

  A character variable name representing the x-axis.

- y:

  A character variable name representing the y-axis.

## Value

A numeric value representing the effective shape parameter.

## Examples

``` r
calculate_effective_shape_value(.data = s_curve_noise_umap, x = "UMAP1", y = "UMAP2")
#> [1] 2.019414
```
