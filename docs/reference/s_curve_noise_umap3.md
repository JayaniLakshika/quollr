# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap3\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap3)
```

## Format

\## \`s_curve_noise_umap3\` A data frame with 75 rows and 3 columns:

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
# Load the s_curve_noise_umap3 dataset
data(s_curve_noise_umap3)

# Display the first few rows of the dataset
head(s_curve_noise_umap3)
#> # A tibble: 6 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#> 1 -2.07   8.13     1
#> 2  4.51  -1.26     2
#> 3  3.63  -3.09     3
#> 4 -4.13   4.61     5
#> 5  0.961  5.46     6
#> 6 -5.94  -6.10     7
```
