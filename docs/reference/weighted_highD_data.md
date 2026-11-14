# Compute Weighted Mean for High-Dimensional Data

This function computes the weighted mean of the specified columns in the
training data based on the distances from the average points in the
non-linear dimensionality reduction (NLDR) space.

## Usage

``` r
weighted_highD_data(
  training_data,
  nldr_df_with_hex_id,
  column_start_text = "x"
)
```

## Arguments

- training_data:

  A data frame containing the training data with an ID column.

- nldr_df_with_hex_id:

  A data frame with 2D embeddings and hexagonal bin IDs.

- column_start_text:

  The starting text of the column names in the training_data that should
  be considered for the weighted mean. Default is "x".

## Value

A data frame with the computed weighted mean for each specified column.
nldr_df_with_hex_id = UMAP_data_with_hb_id, column_start_text = "x")

## Examples

``` r
num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", hex_size = NA, buffer_x = NA)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
 y = "UMAP2", hex_size = NA, buffer_y = NA)
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> Hex size is set to 0.2.
UMAP_data_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))
weighted_highD_data(training_data = s_curve_noise_training,
nldr_df_with_hex_id = UMAP_data_with_hb_id, column_start_text = "x")
#> # A tibble: 10 × 8
#>    hb_id      x1     x2     x3        x4        x5       x6        x7
#>    <dbl>   <dbl>  <dbl>  <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#>  1     2 -0.630  1.74   -1.77   0.00910  -0.00246  -0.00854 -0.00158 
#>  2     6 -0.456  0.443  -1.79  -0.000130  0.00210  -0.0341   0.000214
#>  3     7  0.320  1.47   -1.89   0.00914  -0.00849  -0.0201  -0.00109 
#>  4    12  0.303  0.0545 -1.87   0.00553   0.0119   -0.00321 -0.00288 
#>  5    13  0.883  0.701  -0.755  0.000532 -0.000361  0.0175   0.00205 
#>  6    18  0.334  1.34   -0.148  0.00579   0.000540  0.0103   0.00299 
#>  7    24 -0.802  1.24    0.502 -0.00103   0.00134   0.0157   0.000799
#>  8    28 -0.584  1.21    1.78   0.000158 -0.00400  -0.0145  -0.000303
#>  9    29 -0.0395 0.199   1.95  -0.00361  -0.00255  -0.00902 -0.00213 
#> 10    34  0.643  1.22    1.63  -0.000436  0.00541  -0.00879 -0.00113 
```
