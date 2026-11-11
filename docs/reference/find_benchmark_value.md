# Find Benchmark Value To Remove Long Edges

This function finds the benchmark value to remove long edges based on
the differences in a distance column.

## Usage

``` r
find_benchmark_value(distance_edges, distance_col)
```

## Arguments

- distance_edges:

  The data frame containing the distances.

- distance_col:

  The name of the column containing the distances.

## Value

The benchmark value, which is the first largest difference in the
distance column.

## Examples

``` r
training_data <- s_curve_noise_training
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
UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
df_bin <- avg_highD_data(df_all, column_start_text = "x")
tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#> Warning: triangle collapsed!
#> Warning: triangle collapsed!
#> Warning: three points coincide or are collinear!
#> Warning: three points coincide or are collinear!
tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df,
start_x = "x_from", start_y = "y_from", end_x = "x_to", end_y = "y_to",
select_col_vec = c("from", "to", "distance"))
find_benchmark_value(distance_edges = distance_df, distance_col = "distance")
#> [1] 0.6
```
