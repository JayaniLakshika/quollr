# Visualise the model overlaid on high-dimensional data along with 2-D wireframe model.

This function generates a LangeviTour visualisation based on different
conditions and input parameters with 2-D wireframe.

## Usage

``` r
show_link_plots(
  point_data,
  edge_data,
  point_colours = c("#66B2CC", "#FF7755"),
  point_sizes = c(0, 1)
)
```

## Arguments

- point_data:

  A tibble that contains the high-dimensional data, non-linear dimension
  reductions and model in high-dimensions.

- edge_data:

  A tibble that contains the wireframe data (from and to).

- point_colours:

  A character vector that contains the colours of points in the
  high-dimensional data and model.

- point_sizes:

  A numeric vector that contains the sizes of points in the
  high-dimensional data and model.

## Value

A browsable HTML element.

## Examples

``` r
df_exe <- comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap,
model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d)
edge_data <- scurve_model_obj$trimesh_data
if (interactive()) {
  show_link_plots(point_data = df_exe, edge_data = edge_data)
}
```
