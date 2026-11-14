# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap6\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap6)
```

## Format

\## \`s_curve_noise_umap6\` A data frame with 75 rows and 3 columns:

- UMAP1:

  Numeric, first UMAP 2D embeddings.

- UMAP2:

  Numeric, second UMAP 2D embeddings.

- ID:

  Numeric, identifier for each data point.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the s_curve_noise_umap6 dataset
data(s_curve_noise_umap6)

# Display the first few rows of the dataset
head(s_curve_noise_umap6)
#> # A tibble: 6 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#> 1 -3.31   8.43     1
#> 2  9.44  -2.67     2
#> 3  7.13  -4.73     3
#> 4 -6.02   3.38     5
#> 5  0.621  6.17     6
#> 6 -7.75  -8.18     7
```
