# Visualize triangular mesh with coloured long edges

This function visualize triangular mesh with coloured long edges.

## Usage

``` r
vis_lg_mesh(distance_edges, benchmark_value, tr_coord_df, distance_col)
```

## Arguments

- distance_edges:

  The tibble that contains the edge information.

- benchmark_value:

  The threshold value to determine long edges.

- tr_coord_df:

  A tibble that contains the x and y coordinates of start and end
  points.

- distance_col:

  The column name in \`distance_edges\` representing the distances.

## Value

A ggplot object with the triangular mesh plot where long edges are
coloured differently.

## Examples

``` r
tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
distance_df <- s_curve_obj$distance_df
vis_lg_mesh(distance_edges = distance_df, benchmark_value = 0.75,
tr_coord_df = tr_from_to_df, distance_col = "distance")
#> Error in UseMethod("mutate"): no applicable method for 'mutate' applied to an object of class "NULL"
```
