# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap)
```

## Format

\## \`s_curve_noise_umap\` A data frame with 75 rows and 3 columns:

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
# Load the s_curve_noise_umap dataset
data(s_curve_noise_umap)

# Display the first few rows of the dataset
head(s_curve_noise_umap)
#> # A tibble: 6 × 3
#>    emb1    emb2    ID
#>   <dbl>   <dbl> <int>
#> 1 -6.51 10.1        1
#> 2  7.62 -0.0782     2
#> 3  7.15 -4.08       3
#> 4 -5.53  2.00       5
#> 5  1.10  9.20       6
#> 6 -9.37 -5.71       7
```
