# Find Low-Density Hexagons

This function identifies hexagons with low density based on the mean
density of their neighboring hexagons.

## Usage

``` r
find_low_density_hexagons(
  df_bin_centroids_all,
  num_bins_x,
  df_bin_centroids_low
)
```

## Arguments

- df_bin_centroids_all:

  The data frame containing all hexagonal bin centroids.

- num_bins_x:

  Number of bins along the x-axis for hexagon binning.

- df_bin_centroids_low:

  The data frame containing identified low-density hexagonal bin
  centroids.

## Value

A vector containing the IDs of hexagons to be removed after
investigating their neighboring bins.

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
hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> Hex size is set to 0.2.
all_centroids_df <- as.data.frame(do.call(cbind, hex_bin_obj$full_grid_hex_centroids))
counts_df <- as.data.frame(do.call(cbind, hex_bin_obj$hex_id_with_std_counts))
df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
df_bin_centroids_low <- df_bin_centroids |>
dplyr::filter(std_counts <= 0.43)
find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
df_bin_centroids_low = df_bin_centroids_low)
#> [1] 7
```
