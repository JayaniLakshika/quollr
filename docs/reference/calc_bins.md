# Calculate the effective number of bins along x-axis and y-axis

This function calculates the effective number of bins along the x and y
axes of a hexagonal grid.

## Usage

``` r
calc_bins(data, x, y, hex_size, buffer_x, buffer_y)
```

## Arguments

- data:

  A tibble or data frame.

- x:

  The name of the column that contains values along the x-axis.

- y:

  The name of the column that contains values along the y-axis.

- hex_size:

  A numeric value that initializes the radius of the outer circle
  surrounded by the hexagon.

- buffer_x:

  The buffer size along the x-axis.

- buffer_y:

  The buffer size along the y-axis.

## Value

A list of numeric values that represents the effective number of bins
along the x and y axes of a hexagonal grid.

## Examples

``` r
calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#> $num_x
#> [1] 4
#> 
#> $num_y
#> [1] 7
#> 
```
