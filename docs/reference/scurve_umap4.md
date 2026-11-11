# UMAP embedding for \`scurve\` with n_neighbors = 30 and min_dist = 0.5

The \`scurve_umap4\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap4)
```

## Format

\## \`scurve_umap4\` A data frame with 5000 rows and 3 columns:

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
# Load the scurve_umap4 dataset
data(scurve_umap4)

# Display the first few rows of the dataset
head(scurve_umap4)
#> # A tibble: 6 × 3
#>     emb1   emb2    ID
#>    <dbl>  <dbl> <int>
#> 1 -2.54   6.42      1
#> 2  3.63  -0.405     2
#> 3  3.48  -2.88      3
#> 4 -4.34   6.51      4
#> 5 -3.95   5.89      5
#> 6  0.115  2.80      6
```
