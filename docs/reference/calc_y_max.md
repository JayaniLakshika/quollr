# Compute maximum value of y for scaling

This function compute the maximum y value need to use for scaling.

## Usage

``` r
calc_y_max(aspect_ratio, hex_ratio)
```

## Arguments

- aspect_ratio:

  Numeric value representing the aspect ratio of the plot area.

- hex_ratio:

  Numeric value representing the ratio of the hexagon size.

## Value

A value which should be used as maximum value of y when scaling.

## Examples

``` r
calc_y_max(aspect_ratio = 2.019414, hex_ratio = 0.2309401)
#> [1] 2.078461
```
