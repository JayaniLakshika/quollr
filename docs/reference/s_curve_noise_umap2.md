# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap2\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap2)
```

## Format

\## \`s_curve_noise_umap2\` A data frame with 75 rows and 3 columns:

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
# Load the s_curve_noise_umap2 dataset
data(s_curve_noise_umap2)

# Display the first few rows of the dataset
head(s_curve_noise_umap2)
#> # A tibble: 6 × 3
#>    emb1   emb2    ID
#>   <dbl>  <dbl> <int>
#> 1  5.78 -7.64      1
#> 2 -8.26  0.821     2
#> 3 -6.76  3.86      3
#> 4  5.69 -1.58      5
#> 5 -1.46 -6.36      6
#> 6  7.92  4.56      7
```
