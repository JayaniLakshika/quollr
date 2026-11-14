# Summary with different number of bins for \`scurve_umap3\`

The \`scurve_umap_rmse3\` dataset contains error, RMSE, b2, b, m, a1,
a2, and mean density (d_bar) for different number of bins in x-axis
(b1).

## Usage

``` r
data(scurve_umap_rmse3)
```

## Format

\## \`scurve_umap_rmse3\` A data frame with 70 rows and 10 columns:

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
# Load the scurve_umap_rmse3 dataset
data(scurve_umap_rmse3)

# Display the first few rows of the dataset
head(scurve_umap_rmse3)
#>      Error       HBE b1 b2   b  m   a1   a2    n_bar method
#> 1 580.8831 0.3816431  5 11  55 23 0.27 0.24 43.47826  UMAP3
#> 2 517.9431 0.3409713  6 13  78 31 0.23 0.20 32.25806  UMAP3
#> 3 478.0086 0.3102171  7 15 105 38 0.20 0.17 26.31579  UMAP3
#> 4 446.7043 0.2904024  8 17 136 42 0.17 0.15 23.80952  UMAP3
#> 5 393.9956 0.2530347  9 20 180 55 0.14 0.12 18.18182  UMAP3
#> 6 374.9193 0.2392782 10 22 220 63 0.13 0.11 15.87302  UMAP3
```
