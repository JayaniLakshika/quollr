# Construct the 2D model and lift into high-D

This function fits a high-dimensional model using hexagonal bins and
provides options to customize the modeling process, including the choice
of bin centroids or bin means, removal of low-density hexagons, and
averaging of high-dimensional data.

## Usage

``` r
fit_high_d_model(
  training_data,
  nldr_df_with_id,
  x = "UMAP1",
  y = "UMAP2",
  num_bins_x = NA,
  num_bins_y = NA,
  x_start = NA,
  y_start = NA,
  buffer_x = NA,
  buffer_y = NA,
  hex_size = NA,
  is_rm_lwd_hex = FALSE,
  benchmark_to_rm_lwd_hex = NA,
  column_start_text = "x"
)
```

## Arguments

- training_data:

  A data frame containing the training high-dimensional data.

- nldr_df_with_id:

  A data frame containing 2D embeddings with a unique identifier.

- x:

  The name of the column that contains first 2D embeddings component.

- y:

  The name of the column that contains second 2D embeddings component.

- num_bins_x:

  Number of bins along the x-axis.

- num_bins_y:

  Number of bins along the y-axis.

- x_start:

  Starting point along the x-axis for hexagonal binning.

- y_start:

  Starting point along the y-axis for hexagonal binning.

- buffer_x:

  The buffer size along the x-axis.

- buffer_y:

  The buffer size along the y-axis.

- hex_size:

  A numeric value that initializes the radius of the outer circle
  surrounding the hexagon.

- is_rm_lwd_hex:

  Logical, indicating whether to remove low-density hexagons (default is
  FALSE).

- benchmark_to_rm_lwd_hex:

  The benchmark value to remove low-density hexagons.

- column_start_text:

  The text prefix for columns in the high-dimensional data.

## Value

A list containing the data frame with high-dimensional coordinates for
2D bin centroids (`df_bin`) and the data frame containing information
about hexagonal bin centroids (`df_bin_centroids`) in 2D.

## Examples

``` r
fit_high_d_model(training_data = s_curve_noise_training,
nldr_df_with_id = s_curve_noise_umap_scaled)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> Hex size is set to 0.2.
#> $df_bin
#> # A tibble: 10 × 8
#>    hb_id       x1     x2     x3        x4        x5       x6        x7
#>    <dbl>    <dbl>  <dbl>  <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#>  1     2 -0.637   1.74   -1.76   0.00953  -0.00143  -0.0117  -0.00152 
#>  2     6 -0.498   0.524  -1.73  -0.000237  0.00234  -0.0297  -0.000242
#>  3     7  0.294   1.40   -1.88   0.00890  -0.00803  -0.0123  -0.00120 
#>  4    12  0.309   0.0421 -1.83   0.00656   0.00823   0.00489 -0.00389 
#>  5    13  0.868   0.747  -0.781 -0.00408   0.000857  0.0248   0.00170 
#>  6    18  0.357   1.27   -0.169  0.00607   0.00124   0.0152   0.00204 
#>  7    24 -0.792   1.25    0.514 -0.000777  0.000464  0.00602  0.000371
#>  8    28 -0.597   1.19    1.77   0.000240 -0.00417  -0.0185  -0.000786
#>  9    29 -0.00544 0.211   1.92   0.00116   0.00266   0.00949 -0.00209 
#> 10    34  0.622   1.21    1.64  -0.000560  0.00540  -0.00741 -0.000886
#> 
#> $df_bin_centroids
#>    hexID       c_x   c_y std_counts
#> 1      2 0.1732051 -0.15  0.2352941
#> 2      6 0.0000000  0.15  0.5294118
#> 3      7 0.3464102  0.15  0.4117647
#> 4     12 0.1732051  0.45  0.1764706
#> 5     13 0.5196152  0.45  0.3529412
#> 6     18 0.6928203  0.75  0.7058824
#> 7     24 0.8660254  1.05  0.4705882
#> 8     28 0.6928203  1.35  0.2941176
#> 9     29 1.0392305  1.35  0.2352941
#> 10    34 0.8660254  1.65  1.0000000
#> 
```
