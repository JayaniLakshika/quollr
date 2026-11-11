# Scaled UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap_scaled\` dataset contains the scaled UMAP
(Uniform Manifold Approximation and Projection) embeddings.

## Usage

``` r
data(s_curve_noise_umap_scaled)
```

## Format

\## \`s_curve_noise_umap_scaled\` A data frame with 25 rows and 3
columns:

- UMAP1:

  Numeric, Scaled first UMAP 2D embeddings.

- UMAP2:

  Numeric, Scaled second UMAP 2D embedding.

- ID:

  Numeric, identifier for each data point.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the s_curve_noise_umap_scaled dataset
data(s_curve_noise_umap_scaled)

# Display the first few rows of the dataset
head(s_curve_noise_umap_scaled)
#> # A tibble: 6 × 3
#>    emb1  emb2    ID
#>   <dbl> <dbl> <int>
#> 1 0.270 0.839     1
#> 2 0.788 0.466     2
#> 3 0.771 0.319     3
#> 4 0.306 0.542     5
#> 5 0.549 0.806     6
#> 6 0.166 0.259     7
```
