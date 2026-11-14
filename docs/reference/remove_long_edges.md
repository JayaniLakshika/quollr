# Remove Long Edges from a Triangular Mesh Plot

This function removes long edges from a triangular mesh plot based on a
benchmark value.

## Usage

``` r
remove_long_edges(
  distance_edges,
  benchmark_value,
  tr_from_to_df_coord,
  distance_col
)
```

## Arguments

- distance_edges:

  The data frame containing the edge information.

- benchmark_value:

  The threshold value to determine long edges.

- tr_from_to_df_coord:

  A data frame containing columns for the x and y coordinates of start
  and end points.

- distance_col:

  The column name in \`distance_edges\` representing the distances.

## Value

A ggplot object with the triangular mesh plot where long edges are
removed.

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
df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
 counts_df = counts_df)
tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#> Warning: triangle collapsed!
#> Warning: triangle collapsed!
#> Warning: three points coincide or are collinear!
#> Warning: three points coincide or are collinear!
tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
start_y = "y_from", end_x = "x_to", end_y = "y_to",
select_col_vec = c("from", "to", "distance"))
remove_long_edges(distance_edges = distance_df, benchmark_value = 0.75,
tr_from_to_df_coord = tr_from_to_df, distance_col = "distance")

```
