# Map Polygon ID to Hexagon Coordinates

This function maps polygon IDs to the corresponding hexagon coordinates
in the full grid.

## Usage

``` r
map_polygon_id(full_grid_with_hexbin_id, hex_grid)
```

## Arguments

- full_grid_with_hexbin_id:

  A data frame containing hexagon IDs, centroids, and standardized
  counts.

- hex_grid:

  A data frame containing all coordinates of hexagons.

## Value

A data frame with hexagon information along with mapped polygon IDs.

## Examples

``` r
num_bins_x <- 4
shape_value <- 1.833091
hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
num_bins = num_bins_x, shape_val = shape_value)
df_bin_centroids <- hexbin_data_object$hexdf_data
full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)
hex_grid <- full_hex_grid(full_centroid_df)
full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)
map_polygon_id(full_grid_with_hexbin_id, hex_grid)
#>           c_x        c_y hexID counts std_counts polygon_id
#> 1  -3.2703577 -5.7425405     1      2  0.2222222          1
#> 2  -1.8388696 -5.7425405     2      6  0.6666667          2
#> 3  -0.4073814 -5.7425405     3     NA         NA          3
#> 4   1.0241067 -5.7425405     4     NA         NA          4
#> 5   2.4555949 -5.7425405     5     NA         NA          5
#> 6  -2.5546137 -4.3768263     6      7  0.7777778         26
#> 7  -1.1231255 -4.3768263     7      5  0.5555556         27
#> 8   0.3083626 -4.3768263     8     NA         NA         28
#> 9   1.7398508 -4.3768263     9     NA         NA         29
#> 10  3.1713389 -4.3768263    10     NA         NA         30
#> 11 -3.2703577 -3.0111121    11      2  0.2222222          6
#> 12 -1.8388696 -3.0111121    12      2  0.2222222          7
#> 13 -0.4073814 -3.0111121    13      2  0.2222222          8
#> 14  1.0241067 -3.0111121    14     NA         NA          9
#> 15  2.4555949 -3.0111121    15     NA         NA         10
#> 16 -2.5546137 -1.6453979    16     NA         NA         31
#> 17 -1.1231255 -1.6453979    17      1  0.1111111         32
#> 18  0.3083626 -1.6453979    18      8  0.8888889         33
#> 19  1.7398508 -1.6453979    19     NA         NA         34
#> 20  3.1713389 -1.6453979    20     NA         NA         35
#> 21 -3.2703577 -0.2796838    21     NA         NA         11
#> 22 -1.8388696 -0.2796838    22     NA         NA         12
#> 23 -0.4073814 -0.2796838    23     NA         NA         13
#> 24  1.0241067 -0.2796838    24      7  0.7777778         14
#> 25  2.4555949 -0.2796838    25     NA         NA         15
#> 26 -2.5546137  1.0860304    26     NA         NA         36
#> 27 -1.1231255  1.0860304    27     NA         NA         37
#> 28  0.3083626  1.0860304    28      1  0.1111111         38
#> 29  1.7398508  1.0860304    29      6  0.6666667         39
#> 30  3.1713389  1.0860304    30     NA         NA         40
#> 31 -3.2703577  2.4517446    31     NA         NA         16
#> 32 -1.8388696  2.4517446    32     NA         NA         17
#> 33 -0.4073814  2.4517446    33     NA         NA         18
#> 34  1.0241067  2.4517446    34     NA         NA         19
#> 35  2.4555949  2.4517446    35     NA         NA         20
#> 36 -2.5546137  3.8174588    36     NA         NA         41
#> 37 -1.1231255  3.8174588    37     NA         NA         42
#> 38  0.3083626  3.8174588    38      2  0.2222222         43
#> 39  1.7398508  3.8174588    39      8  0.8888889         44
#> 40  3.1713389  3.8174588    40     NA         NA         45
#> 41 -3.2703577  5.1831729    41     NA         NA         21
#> 42 -1.8388696  5.1831729    42     NA         NA         22
#> 43 -0.4073814  5.1831729    43     NA         NA         23
#> 44  1.0241067  5.1831729    44      9  1.0000000         24
#> 45  2.4555949  5.1831729    45      7  0.7777778         25
#> 46 -2.5546137  6.5488871    46     NA         NA         46
#> 47 -1.1231255  6.5488871    47     NA         NA         47
#> 48  0.3083626  6.5488871    48     NA         NA         48
#> 49  1.7398508  6.5488871    49     NA         NA         49
#> 50  3.1713389  6.5488871    50     NA         NA         50
```
