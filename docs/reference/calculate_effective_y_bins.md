# Calculate the effective number of bins along y-axis

This function calculates the effective number of bins along the y-axis
of a hexagonal grid.

## Usage

``` r
calculate_effective_y_bins(nldr_df, y = "UMAP2", hex_size = NA, buffer_y = NA)
```

## Arguments

- nldr_df:

  A data frame containing 2D embeddings.

- y:

  The name of the column that contains second 2D embeddings component.

- hex_size:

  A numeric value that initializes the radius of the outer circle
  surrounding the hexagon.

- buffer_y:

  The buffer size along the y-axis.

## Value

A numeric value representing the effective number of bins along x-axis.

## Examples

``` r
calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled, y = "UMAP2",
hex_size = NA, buffer_y = NA)
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
#> [1] 8
```
