# Generate Edge Information

This function generates edge information from a given triangular object,
including the coordinates of the vertices and the from-to relationships
between the vertices.

## Usage

``` r
generate_edge_info(triangular_object)
```

## Arguments

- triangular_object:

  The triangular object from which to generate edge information.

## Value

A data frame containing the edge information, including the from-to
relationships and the corresponding x and y coordinates.

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
centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2",
                                             num_bins_x = num_bins_x,
                                             num_bins_y = num_bins_y,
                                             x_start = NA, y_start = NA,
                                             buffer_x = NA,
                                             buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
centroid_df = all_centroids_df)
umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
std_counts_list <- compute_std_counts(nldr_df_with_hex_id = umap_with_hb_id)
counts_df <- as.data.frame(do.call(cbind, std_counts_list))
df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#> Warning: triangle collapsed!
#> Warning: triangle collapsed!
#> Warning: three points coincide or are collinear!
#> Warning: three points coincide or are collinear!
generate_edge_info(triangular_object = tr1_object)
#> # A tibble: 21 × 6
#>     from    to x_from y_from  x_to  y_to
#>    <int> <int>  <dbl>  <dbl> <dbl> <dbl>
#>  1     1     3  0.173  -0.15 0.346  0.15
#>  2     2     4  0       0.15 0.173  0.45
#>  3     4     5  0.173   0.45 0.520  0.45
#>  4     3     5  0.346   0.15 0.520  0.45
#>  5     4     6  0.173   0.45 0.693  0.75
#>  6     6     7  0.693   0.75 0.866  1.05
#>  7     1     7  0.173  -0.15 0.866  1.05
#>  8     4     8  0.173   0.45 0.693  1.35
#>  9     6     8  0.693   0.75 0.693  1.35
#> 10     8     9  0.693   1.35 1.04   1.35
#> # ℹ 11 more rows
```
