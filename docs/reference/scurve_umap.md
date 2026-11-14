# UMAP embedding for \`scurve\` with n_neighbors = 15 and min_dist = 0.1

The \`scurve_umap\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap)
```

## Format

\## \`scurve_umap\` A data frame with 5000 rows and 3 columns:

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
# Load the scurve_umap dataset
data(scurve_umap)

# Display the first few rows of the dataset
head(scurve_umap)
#> # A tibble: 6 × 3
#>    emb1   emb2    ID
#>   <dbl>  <dbl> <int>
#> 1 -4.24  5.80      1
#> 2  3.19 -0.851     2
#> 3  4.65 -3.31      3
#> 4 -6.08  6.50      4
#> 5 -5.29  7.03      5
#> 6  1.35  8.19      6
```
