# Generate Evaluation Data Frame

This function generates an evaluation data frame based on the provided
data and predictions.

## Usage

``` r
generate_eval_df(test_data, prediction_df, df_bin, col_start = "x")
```

## Arguments

- test_data:

  The data set containing high-dimensional data along with an unique
  identifier.

- prediction_df:

  The data set with 2D embeddings, IDs, and predicted hexagonal IDs.

- df_bin:

  The data set with averaged/weighted high-dimensional data.

- col_start:

  The text that begin the column name of the high-D data

## Value

A list contains MSE and AIC values.

## Examples

``` r
training_data <- s_curve_noise_training
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
all_centroids_df <- as.data.frame(do.call(cbind, hex_bin_obj$full_grid_hex_centroids))
counts_df <- as.data.frame(do.call(cbind, hex_bin_obj$hex_id_with_std_counts))
df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
df_bin <- avg_highD_data(df_all, column_start_text = "x")
pred_emb_list <- predict_2d_embeddings(test_data = s_curve_noise_training,
df_bin_centroids = df_bin_centroids, df_bin = df_bin, type_NLDR = "UMAP")
pred_df_test <- as.data.frame(do.call(cbind, pred_emb_list))
generate_eval_df(test_data = s_curve_noise_training, prediction_df = pred_df_test,
df_bin = df_bin, col_start = "x")
#> $mse
#> [1] 0.3229922
#> 
#> $aic
#> [1] -453.3167
#> 
```
