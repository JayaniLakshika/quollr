# Generate evaluation metrics

This function generates an evaluation data frame based on the provided
data and predictions.

## Usage

``` r
gen_summary(test_data, prediction_df, df_bin, col_start = "x")
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

  The text that begin the column name of the high-dimensional data.

## Value

A list contains Error, MSE and AIC values.

## Examples

``` r
model <- fit_highd_model(training_data = s_curve_noise_training, x = "UMAP1", y = "UMAP2",
nldr_df_with_id = s_curve_noise_umap_scaled, col_start_2d = "UMAP", col_start_highd = "x")
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
#> Buffer along the y-axis is set to 0.45.
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
#> Hex size is set to 0.2.
df_bin_centroids <- model$df_bin_centroids
df_bin <- model$df_bin
pred_emb_list <- predict_emb(test_data = s_curve_noise_training,
df_bin_centroids = df_bin_centroids, df_bin = df_bin, type_NLDR = "UMAP")
pred_df_test <- as.data.frame(do.call(cbind, pred_emb_list))
gen_summary(test_data = s_curve_noise_training, prediction_df = pred_df_test,
df_bin = df_bin, col_start = "x")
#> $error
#> [1] 63.83385
#> 
#> $mse
#> [1] 0.3229922
#> 
#> $aic
#> [1] -453.3167
#> 
```
