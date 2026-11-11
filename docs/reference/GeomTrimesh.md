# GeomTrimesh: A Custom ggplot2 Geom for Triangular Meshes

This function defines a custom ggplot2 Geom, GeomTrimesh, for rendering
triangular meshes.

## Usage

``` r
GeomTrimesh
```

## Format

A ggproto object

## Details

\- `required_aes`: The required aesthetics for this geometry are `"x"`,
`"y"`, `"xend"`, and `"yend"`. - `default_aes`: The default aesthetics
for this geometry include `shape = 19`, `linetype = 1`,
`linewidth = 0.5`, `size = 0.5`, `alpha = NA`, and `colour = "black"`. -
`draw_key`: The function describing how to draw the key glyph is
[`ggplot2::draw_key_point`](https://ggplot2.tidyverse.org/reference/draw_key.html). -
`draw_panel`: The function describing how to draw the panel takes
`data`, `panel_scales`, and `coord`. It creates a tibble of `vertices`
and a tibble of `trimesh`. The final plot is constructed using
`ggplot2::GeomPoint$draw_panel` for vertices and
`ggplot2::GeomSegment$draw_panel` for trimesh.
