# Arrange RMSE plot and 2-D layouts

This function arranges RMSE plot in left and 2-D layouts in right.

## Usage

``` r
plot_rmse_layouts(plots, design)
```

## Arguments

- plots:

  A list of plots which include RMSE plot and 2-D layouts.

- design:

  The design of plots need to be arranged.

## Value

A patchwork object.

## Examples

``` r
design <- gen_design(n_right = 4, ncol_right = 2)
plot_rmse_layouts(plots = scurve_plts, design = design)

```
