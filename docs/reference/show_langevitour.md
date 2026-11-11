# Visualise the model overlaid on high-dimensional data

This function generates a langevitour which visualise the model overlaid
on high-dimensional data.

## Usage

``` r
show_langevitour(
  point_data,
  edge_data,
  point_colours = c("#66B2CC", "#FF7755"),
  point_sizes = c(2, 1)
)
```

## Arguments

- point_data:

  A tibble that contains the high-dimensional data and model in
  high-dimensions.

- edge_data:

  A tibble that contains the wireframe data (from and to).

- point_colours:

  A character vector that contains the colours of points in the
  high-dimensional data and model.

- point_sizes:

  A numeric vector that contains the sizes of points in the
  high-dimensional data and model.

## Value

A langevitour object with the model and the high-dimensional data.

## Examples

``` r
df_exe <- comb_data_model(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
model_2d = scurve_model_obj$model_2d)
edge_data <- scurve_model_obj$trimesh_data
if (interactive()) {
  show_langevitour(point_data = df_exe, edge_data = edge_data)
}
```
