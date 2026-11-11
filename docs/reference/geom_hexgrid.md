# Create a hexgrid plot

Create a hexgrid plot

## Usage

``` r
geom_hexgrid(
  mapping = NULL,
  data = NULL,
  stat = "hexgrid",
  position = "identity",
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Aesthetic mappings for the plot.

- data:

  The data to be plotted.

- stat:

  The statistical transformation to be applied.

- position:

  The position adjustment to be applied.

- show.legend:

  Whether to show the legend for this layer.

- na.rm:

  Whether to remove missing values.

- inherit.aes:

  Whether to inherit aesthetics from the plot or the layer.

- ...:

  Additional arguments to be passed to the \`layer\` function.

## Value

A \`ggplot2\` layer object.

## Examples

``` r
df_bin_centroids <- scurve_model_obj$model_2d |> dplyr::filter(n_h > 10)
ggplot2::ggplot() +
geom_hexgrid(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y))

```
