# Predicted UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap_predict\` dataset contains the predicted UMAP
(Uniform Manifold Approximation and Projection) embeddings of a
three-dimensional S-curve with added noise. Each data point is
represented by two UMAP coordinates (UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap_predict)
```

## Format

\## \`s_curve_noise_umap_predict\` A data frame with 75 rows and 3
columns:

- UMAP1:

  Numeric, predicted first UMAP 2D embeddings.

- UMAP2:

  Numeric, predicted second UMAP 2D embeddings.

- ID:

  Numeric, identifier for each data point.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the s_curve_noise_umap_predict dataset
data(s_curve_noise_umap_predict)

# Display the first few rows of the dataset
head(s_curve_noise_umap_predict)
#> # A tibble: 6 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#> 1 -6.40   3.50     4
#> 2  9.55  -3.05     8
#> 3 -5.77  -6.76    13
#> 4  5.56   3.99    14
#> 5 -6.15   8.82    17
#> 6  0.342 -5.10    20
```
