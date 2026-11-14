# UMAP embedding for \`scurve\` with n_neighbors = 10 and min_dist = 0.4

The \`scurve_umap2\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap2)
```

## Format

\## \`scurve_umap2\` A data frame with 5000 rows and 3 columns:

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
# Load the scurve_umap2 dataset
data(scurve_umap2)

# Display the first few rows of the dataset
head(scurve_umap2)
#> # A tibble: 6 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#> 1  3.32  -6.31     1
#> 2 -9.39   5.09     2
#> 3 -6.51   5.76     3
#> 4  6.10  -5.03     4
#> 5  5.71  -4.76     5
#> 6 -0.255 -2.95     6
```
