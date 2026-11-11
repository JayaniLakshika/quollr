# UMAP embedding for \`scurve\` with n_neighbors = 62 and min_dist = 0.1

The \`scurve_umap3\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap3)
```

## Format

\## \`scurve_umap3\` A data frame with 5000 rows and 3 columns:

- UMAP1:

  Numeric, first UMAP 2-D embeddings.

- UMAP2:

  Numeric, second UMAP 2-D embeddings.

- ID:

  Numeric, identifier for each data point.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the scurve_umap3 dataset
data(scurve_umap3)

# Display the first few rows of the dataset
head(scurve_umap3)
#> # A tibble: 6 × 3
#>    emb1  emb2    ID
#>   <dbl> <dbl> <int>
#> 1 -1.89  5.98     1
#> 2  4.21 -1.80     2
#> 3  3.15 -3.03     3
#> 4 -2.91  6.98     4
#> 5 -2.56  7.17     5
#> 6  1.08  5.87     6
```
