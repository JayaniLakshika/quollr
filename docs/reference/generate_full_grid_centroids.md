# Generate Full Grid Centroids Coordinates

This function generates all possible centroids in the full grid based on
2D embeddings.

## Usage

``` r
generate_full_grid_centroids(
  nldr_df,
  x = "UMAP1",
  y = "UMAP2",
  num_bins_x,
  num_bins_y,
  x_start = NA,
  y_start = NA,
  buffer_x = NA,
  buffer_y = NA,
  hex_size = NA
)
```

## Arguments

- nldr_df:

  A data frame containing 2D embeddings.

- x:

  The name of the column that contains first 2D embeddings component.

- y:

  The name of the column that contains second 2D embeddings component.

- num_bins_x:

  Number of bins along the x-axis.

- num_bins_y:

  Number of bins along the y-axis.

- x_start:

  Starting point along the x-axis for hexagonal binning.

- y_start:

  Starting point along the y-axis for hexagonal binning.

- buffer_x:

  The buffer size along the x-axis.

- buffer_y:

  The buffer size along the y-axis.

- hex_size:

  A numeric value that initializes the radius of the outer circle
  surrounding the hexagon.

## Value

A list contains hexIDs, x and y coordinates (hexID, c_x, c_y
respectively) of all hexagon bin centroids.

## Examples

``` r
num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", hex_size = NA, buffer_x = NA)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
 y = "UMAP2", hex_size = NA, buffer_y = NA)
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> $hexID
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
#> 
#> $c_x
#>  [1] -0.1732051  0.1732051  0.5196152  0.8660254  1.2124356  0.0000000
#>  [7]  0.3464102  0.6928203  1.0392305  1.3856406 -0.1732051  0.1732051
#> [13]  0.5196152  0.8660254  1.2124356  0.0000000  0.3464102  0.6928203
#> [19]  1.0392305  1.3856406 -0.1732051  0.1732051  0.5196152  0.8660254
#> [25]  1.2124356  0.0000000  0.3464102  0.6928203  1.0392305  1.3856406
#> [31] -0.1732051  0.1732051  0.5196152  0.8660254  1.2124356  0.0000000
#> [37]  0.3464102  0.6928203  1.0392305  1.3856406
#> 
#> $c_y
#>  [1] -0.15 -0.15 -0.15 -0.15 -0.15  0.15  0.15  0.15  0.15  0.15  0.45  0.45
#> [13]  0.45  0.45  0.45  0.75  0.75  0.75  0.75  0.75  1.05  1.05  1.05  1.05
#> [25]  1.05  1.35  1.35  1.35  1.35  1.35  1.65  1.65  1.65  1.65  1.65  1.95
#> [37]  1.95  1.95  1.95  1.95
#> 
```
