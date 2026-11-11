# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap4\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap4)
```

## Format

\## \`s_curve_noise_umap4\` A data frame with 75 rows and 3 columns:

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
# Load the s_curve_noise_umap4 dataset
data(s_curve_noise_umap4)

# Display the first few rows of the dataset
head(s_curve_noise_umap4)
#> # A tibble: 6 × 3
#>    emb1  emb2    ID
#>   <dbl> <dbl> <int>
#> 1  4.19 -7.99     1
#> 2 -8.72  1.94     2
#> 3 -6.24  4.58     3
#> 4  6.68 -7.98     5
#> 5 -1.00 -5.75     6
#> 6  7.74  9.30     7
```
