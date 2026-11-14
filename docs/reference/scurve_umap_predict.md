# Predicted UMAP embedding for \`scurve\` data

The \`scurve_umap_predict\` dataset contains the predicted UMAP (Uniform
Manifold Approximation and Projection) embeddings of a three-dimensional
S-curve with added noise. Each data point is represented by two UMAP
coordinates (UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(scurve_umap_predict)
```

## Format

\## \`scurve_umap_predict\` A data frame with 5000 rows and 3 columns:

- UMAP1:

  Numeric, predicted first UMAP 2-D embeddings.

- UMAP2:

  Numeric, predicted second UMAP 2-D embeddings.

- ID:

  Numeric, identifier for each data point.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the scurve_umap_predict dataset
data(scurve_umap_predict)

# Display the first few rows of the dataset
head(scurve_umap_predict)
#> # A tibble: 6 × 3
#>    emb1   emb2    ID
#>   <dbl>  <dbl> <int>
#> 1 -4.36  5.70      1
#> 2  2.22 -0.925     2
#> 3  4.46 -3.06      3
#> 4 -5.94  6.48      4
#> 5 -5.24  6.94      5
#> 6  1.54  8.25      6
```
