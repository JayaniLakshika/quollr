# UMAP embedding for \`scurve\` with n_neighbors = 46 and min_dist = 0.9

The \`scurve_umap6\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap6)
```

## Format

\## \`scurve_umap6\` A data frame with 5000 rows and 3 columns:

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
# Load the scurve_umap6 dataset
data(scurve_umap6)

# Display the first few rows of the dataset
head(scurve_umap6)
#> # A tibble: 6 × 3
#>     emb1   emb2    ID
#>    <dbl>  <dbl> <int>
#> 1 -3.94  -8.86      1
#> 2  7.09   0.591     2
#> 3  6.01   3.34      3
#> 4 -6.20  -3.59      4
#> 5 -5.90  -3.46      5
#> 6 -0.190 -6.56      6
```
