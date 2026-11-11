# Visualize triangular mesh after removing the long edges

This function visualize the triangular mesh after removing the long
edges.

## Usage

``` r
vis_rmlg_mesh(tr_coord_df)
```

## Arguments

- tr_coord_df:

  A tibble that contains the x and y coordinates of start and end
  points.

## Value

A ggplot object with the triangular mesh plot where long edges are
removed.

## Examples

``` r
tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
vis_rmlg_mesh(tr_coord_df = tr_from_to_df)

```
