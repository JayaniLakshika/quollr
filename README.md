
# quollr: questioning how a high-dimensional object looks in low-dimensions using r

<!--<img src="man/figures/logo.png" align="right" height="139" alt="" />-->

quollr constructs low-dimensional manifolds by combining Non-Linear
Dimension Reduction (NLDR), hexagonal binning, and triangulation
techniques, enabling accurate high-dimensional data representations. The
package integrates these manifolds with original data using the tour
technique, while also offering tools for generating simulated data sets
for comprehensive exploration.

To start with quollr, refer to the package documentation and explore its
functions

<!--An associated paper can be found at-->

## Installation

You can install the development version of quollr from GitHub with:

``` r
#install.packages("remotes")
#remotes::install_github("JayaniLakshika/quollr")
```

## Usage

The package’s functionalities as follows:

To begin, you need to load the high-dimensional data and 2D embeddings.

``` r
training_data <- readr::read_rds(file = paste0(here::here(), "/data/s_curve_noise_training.rds"))
UMAP_data <- readr::read_rds(file = paste0(here::here(), "/data/s_curve_noise_umap.rds"))
```

``` r
library(quollr)
library(ggplot2)
```

1.  **Optimize Hexagonal Grid**: Determine the optimal hexagonal grid
    configuration by calculating the number of bins and shape parameter
    using `calculate_effective_x_bins()` and
    `calculate_effective_shape_value()`.

``` r
num_bins_x <- calculate_effective_x_bins(.data = UMAP_data, x = UMAP1,
                           cell_area = 1)

shape_val <- calculate_effective_shape_value(.data = UMAP_data, 
                                             x = UMAP1, y = UMAP2)
```

2.  **Generate Hexagonal Object**: Use the calculated parameters to
    generate a hexagonal object, representing the spatial distribution
    of data points.

``` r
hb_object <- hexbin::hexbin(x = UMAP_data |> dplyr::pull(UMAP1), 
                     y = UMAP_data |> dplyr::pull(UMAP2), 
                     xbins = num_bins_x, IDs = TRUE, 
                     shape = shape_val)
```

3.  **Create Hex Bin Plot**: Visualize the hex bin plot to observe data
    density within each hex bin.

4.  **Assign Data Observations to Hex Bins**: Assign high-dimensional
    data points to corresponding hex bins using the `create_hexbin()`
    function.

``` r
hexbin <- create_hexbin(.data = training_data, nldr_df = UMAP_data, 
                        embedding_1 = UMAP1, embedding_2 = UMAP2, 
                        num_bins = num_bins_x, shape_val = shape_val, 
                        apply_pca = FALSE)

df_all <- hexbin$df
hb_object <- hexbin$hb
```

5.  **Compute High-Dimensional Average**: Compute the average of
    high-dimensional data for each hex bin.

``` r
df_bin <- avg_highD_data(.data = df_all, apply_pca = FALSE)
```

6.  **Extract Bin Centroids**: Extract bin centroids for further
    processing.

``` r
df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)
```

7.  **Address Low-Density Hexagons**: Filter out low-density hexagons to
    enhance data representation.

``` r
min_std_cell_threshold <- 0.05
df_bin_centroids <- df_bin_centroids |>
  dplyr::mutate(stand_cell_count = Cell_count/max(Cell_count)) |>
  dplyr::filter(stand_cell_count > min_std_cell_threshold)
```

8.  **Triangulate Bin Centroids**: Triangulate the centroids of hex
    bins.

``` r
tr1_object <- triangulate_bin_centroids(df_bin_centroids)
```

9.  **Generate Edge Information**: Create a dataset containing edge
    information for each triangle.

``` r
tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
```

10. **Calculate 2D Distances**: Calculate 2D distances between edges.

``` r
distance <- cal_2D_dist(.data = tr_from_to_df)
```

11. **Visualize Triangulated Mesh**: Visualize the triangulated mesh in
    low-dimensional space.

``` r
trimesh <- ggplot(df_bin_centroids, aes(x = x_val_center, y = y_val_center)) + 
  geom_point(size = 0.1) + 
  geom_trimesh() + 
  coord_equal() 

trimesh
```

    ## Warning: Computation failed in `stat_trimesh()`
    ## Caused by error in `tr_df %>% dplyr::mutate(ID = dplyr::row_number())`:
    ## ! could not find function "%>%"

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

12. **Remove Long Edges**: Enhance the representation by removing long
    edges.

``` r
benchmark <- find_benchmark_value(.data = distance, distance_col = distance)
trimesh_gr <- colour_long_edges(.data = distance, benchmark_value = benchmark, 
                 triangular_object = tr1_object, distance_col = distance) 

trimesh_gr
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

13. **Tour View**: Visualize the tour that simultaneously displays the
    original and model data along with the learned low-dimensional
    manifold in high-dimensional space.

``` r
show_langevitour(df_all, df_bin, df_bin_centroids, benchmark_value = benchmark, distance, distance)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

By following these steps, you can effectively leverage **quollr** to
construct informative low-dimensional representations of
high-dimensional data, while also benefitting from its simulated data
generation functions to explore various scenarios and conditions.

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/quollr/LICENSE.md).
