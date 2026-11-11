# Summary with different number of bins for \`scurve_umap2\`

The \`scurve_umap_rmse2\` dataset contains error, RMSE, b2, b, m, a1,
a2, and mean density (d_bar) for different number of bins in x-axis
(b1).

## Usage

``` r
data(scurve_umap_rmse2)
```

## Format

\## \`scurve_umap_rmse2\` A data frame with 70 rows and 10 columns:

- Error:

  Numeric, model error.

- RMSE:

  Numeric, Root Mean Square Error.

- b1:

  Numeric, number of bins along x-axis.

- b2:

  Numeric, number of bins along y-axis.

- b:

  Numeric, number of total bins.

- m:

  Numeric, number of non-empty bins.

- a1:

  Numeric, hexagon bin width.

- a2:

  Numeric, hexagon bin height.

- d_bar:

  Numeric, mean desnity.

- method:

  Character, NLDR method.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the scurve_umap_rmse2 dataset
data(scurve_umap_rmse2)

# Display the first few rows of the dataset
head(scurve_umap_rmse2)
#>      Error       HBE b1 b2   b  m   a1   a2    n_bar method
#> 1 678.3037 0.4500205  5  6  30 19 0.27 0.23 52.63158  UMAP2
#> 2 612.6720 0.4062597  6  7  42 22 0.22 0.19 45.45455  UMAP2
#> 3 540.2149 0.3487553  7  8  56 30 0.19 0.16 33.33333  UMAP2
#> 4 472.2827 0.2985088  8  9  72 37 0.17 0.14 27.02703  UMAP2
#> 5 450.7841 0.2892101  9 10  90 43 0.15 0.13 23.25581  UMAP2
#> 6 407.4070 0.2574744 10 11 110 52 0.13 0.12 19.23077  UMAP2
```
