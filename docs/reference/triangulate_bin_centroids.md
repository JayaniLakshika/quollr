# Triangulate Bin Centroids

This function triangulates the bin centroids/ means using the x and y
coordinates provided in the input data frame and returns the triangular
object.

## Usage

``` r
triangulate_bin_centroids(hex_bin_df, x = "c_x", y = "c_y")
```

## Arguments

- hex_bin_df:

  The data frame containing the bin centroids/ means.

- x:

  The name of the column that contains x coordinates of bin centroids/
  means.

- y:

  The name of the column that contains y coordinates of bin centroids/
  means.

## Value

A triangular object representing the triangulated bin centroids/ means.

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
triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#> Warning: triangle collapsed!
#> Warning: triangle collapsed!
#> Warning: three points coincide or are collinear!
#> Warning: three points coincide or are collinear!
#> Delaunay triangulation, node and triangle indices:
#> triangle: nodes (a,b,c), neighbour triangles [i,j,k] 
#> 1: (1,3,2), [2,0,4]
#> 2: (4,2,3), [1,3,9]
#> 3: (5,4,3), [2,4,5]
#> 4: (5,3,1), [1,8,3]
#> 5: (6,4,5), [3,7,10]
#> 6: (6,7,8), [11,10,7]
#> 7: (7,6,5), [5,8,6]
#> 8: (7,1,5), [4,7,0]
#> 9: (8,4,2), [2,0,10]
#> 10: (6,8,4), [9,5,6]
#> 11: (9,8,7), [6,0,12]
#> 12: (10,8,9), [11,0,0]
#> boundary nodes:  2 1 7 9 10 8 
```
