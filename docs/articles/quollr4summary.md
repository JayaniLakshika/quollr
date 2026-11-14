# 4. Generating model summaries

This demonstrates how to generate and inspect model summaries.
Summarising models fitted to both the high-dimensional space and its
corresponding 2-D embedding is an essential step in evaluating how well
a low-dimensional representation captures the structure of the original
data.

``` r

library(quollr)
library(dplyr)
library(ggplot2)
```

## Step 1: Fitting the model

Begin by fitting a high-dimensional model and its corresponding 2-D
model using the
[`fit_highd_model()`](https://jayanilakshika.github.io/quollr/reference/fit_highd_model.md)
function. This generates the 2-D bin centroids (the 2-D model) and their
corresponding coordinates in the high-dimensional space (the lifted
model).

``` r

model <- fit_highd_model(
  highd_data = scurve, 
  nldr_data = scurve_umap, 
  b1 = 4, 
  q = 0.1, 
  benchmark_highdens = 5
)

df_bin_centroids <- model$model_2d
df_bin <- model$model_highd
```

## Step 2: Predicting 2-D embedding for data

To evaluate model fit, you can predict the 2-D embedding for each
observation in the original high-dimensional dataset.

``` r

pred_df_training <- predict_emb(
  highd_data = scurve, 
  model_highd = scurve_model_obj$model_highd,
  model_2d = scurve_model_obj$model_2d
)

glimpse(pred_df_training)
#> Rows: 5,000
#> Columns: 4
#> $ pred_emb_1 <dbl> 0.6909829, 0.1914148, 0.3163068, 0.7742443, 0.7742443, 0.44…
#> $ pred_emb_2 <dbl> 0.84814779, 0.41550908, 0.19918973, 0.55972199, 0.55972199,…
#> $ ID         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
#> $ pred_h     <int> 205, 109, 66, 146, 146, 172, 58, 110, 141, 96, 109, 102, 72…
```

### Visualising predictions

The plot below shows the original UMAP embedding of the training data in
grey, overlaid with the predicted 2-D coordinates in red.

``` r

umap_scaled <- scurve_model_obj$nldr_obj$scaled_nldr

umap_scaled |>
  ggplot(aes(x = emb1, y = emb2, label = ID)) +
  geom_point(alpha = 0.5) +
  geom_point(data = pred_df_training, aes(x = pred_emb_1, y = pred_emb_2), 
             color = "red", alpha = 0.5) +
  coord_equal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 7)
  )
```

![UMAP embedding of the S-curve training data with predictions in
red.](quollr4summary_files/figure-html/unnamed-chunk-4-1.png)

## Step 3: Computing model summaries

Use the
[`glance()`](https://jayanilakshika.github.io/quollr/reference/glance.md)
function to compute summary statistics that describe how well the 2-D
model captures structure in the high-dimensional space.

``` r

glance(
  highd_data = scurve, 
  model_highd = scurve_model_obj$model_highd,
  model_2d = scurve_model_obj$model_2d
)
#> # A tibble: 1 × 2
#>   Error  RMSE
#>   <dbl> <dbl>
#> 1 1554. 0.190
```

## Step 4: Augmenting the dataset

To obtain a detailed data frame that includes the high-dimensional
observations, their assigned bins, predicted embeddings, and summary
metrics, use the
[`augment()`](https://jayanilakshika.github.io/quollr/reference/augment.md)
function:

``` r

augment(
  highd_data = scurve, 
  model_highd = scurve_model_obj$model_highd,
  model_2d = scurve_model_obj$model_2d
) |>
  head(5)
#> # A tibble: 5 × 32
#>      ID      x1     x2       x3       x4       x5      x6        x7 pred_h
#>   <int>   <dbl>  <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl>  <int>
#> 1     1 -0.120  1.64   -1.99     0.0104   0.0125   0.0923 -0.00128     205
#> 2     2 -0.0492 1.51    0.00121 -0.0177   0.00726 -0.0362 -0.00535     109
#> 3     3 -0.774  1.30    0.367   -0.00173  0.0156  -0.0962  0.00335      66
#> 4     4 -0.606  0.246  -1.80    -0.00897 -0.0187  -0.0716  0.00126     146
#> 5     5 -0.478  0.0177 -1.88     0.00848  0.00533  0.0998  0.000677    146
#> # ℹ 23 more variables: model_high_d_x1 <dbl>, model_high_d_x2 <dbl>,
#> #   model_high_d_x3 <dbl>, model_high_d_x4 <dbl>, model_high_d_x5 <dbl>,
#> #   model_high_d_x6 <dbl>, model_high_d_x7 <dbl>, error_square_x1 <dbl>,
#> #   error_square_x2 <dbl>, error_square_x3 <dbl>, error_square_x4 <dbl>,
#> #   error_square_x5 <dbl>, error_square_x6 <dbl>, error_square_x7 <dbl>,
#> #   row_wise_total_error <dbl>, abs_error_x1 <dbl>, abs_error_x2 <dbl>,
#> #   abs_error_x3 <dbl>, abs_error_x4 <dbl>, abs_error_x5 <dbl>, …
```
