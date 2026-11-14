# Calculate the effective number of bins along x-axis

This function calculates the effective number of bins along the x-axis
of a hexagonal grid.

## Usage

``` r
calculate_effective_x_bins(nldr_df, x = "UMAP1", hex_size = NA, buffer_x = NA)
```

## Arguments

- nldr_df:

  A data frame containing 2D embeddings.

- x:

  The name of the column that contains first 2D embeddings component.

- hex_size:

  A numeric value that initializes the radius of the outer circle
  surrounding the hexagon.

- buffer_x:

  The buffer size along the x-axis.

## Value

A numeric value representing the effective number of bins along x-axis.

## Examples

``` r
calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled, x = "UMAP1",
hex_size = NA, buffer_x = NA)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
#> [1] 5
```
