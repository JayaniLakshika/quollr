# UMAP embedding for S-curve dataset which with noise dimensions

The \`s_curve_noise_umap5\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve
with added noise. Each data point is represented by two UMAP coordinates
(UMAP1 and UMAP2) and an ID.

## Usage

``` r
data(s_curve_noise_umap5)
```

## Format

\## \`s_curve_noise_umap5\` A data frame with 75 rows and 3 columns:

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
# Load the s_curve_noise_umap5 dataset
data(s_curve_noise_umap5)

# Display the first few rows of the dataset
head(s_curve_noise_umap5)
#> # A tibble: 6 × 3
#>     emb1   emb2    ID
#>    <dbl>  <dbl> <int>
#> 1  -5.22  6.39      1
#> 2   8.18 -0.255     2
#> 3   6.36 -3.32      3
#> 4  -6.01  7.60      5
#> 5   1.38  7.93      6
#> 6 -10.2  -7.36      7
```
