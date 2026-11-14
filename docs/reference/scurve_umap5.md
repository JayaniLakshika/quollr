# UMAP embedding for \`scurve\` with n_neighbors = 15 and min_dist = 0.5

The \`scurve_umap5\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap5)
```

## Format

\## \`scurve_umap5\` A data frame with 5000 rows and 3 columns:

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
# Load the scurve_umap5 dataset
data(scurve_umap5)

# Display the first few rows of the dataset
head(scurve_umap5)
#> # A tibble: 6 × 3
#>     emb1  emb2    ID
#>    <dbl> <dbl> <int>
#> 1 -5.33  -9.79     1
#> 2 11.4    3.02     2
#> 3  8.27   5.66     3
#> 4 -7.91  -6.87     4
#> 5 -7.17  -6.40     5
#> 6  0.488 -7.92     6
```
