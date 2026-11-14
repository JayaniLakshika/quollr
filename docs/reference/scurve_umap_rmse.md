# Summary with different number of bins for \`scurve_umap\`

The \`scurve_umap_rmse\` dataset contains error, RMSE, b2, b, m, a1, a2,
and mean density (d_bar) for different number of bins in x-axis (b1).

## Usage

``` r
data(scurve_umap_rmse)
```

## Format

\## \`scurve_umap_rmse\` A data frame with 70 rows and 10 columns:

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
# Load the scurve_umap_rmse dataset
data(scurve_umap_rmse)

# Display the first few rows of the dataset
head(scurve_umap_rmse)
#>      Error       HBE b1 b2   b  m   a1   a2    n_bar method
#> 1 628.8048 0.4098319  5  7  35 22 0.26 0.23 45.45455   UMAP
#> 2 562.7530 0.3672331  6  8  48 27 0.23 0.20 37.03704   UMAP
#> 3 520.4474 0.3364583  7  9  63 33 0.20 0.17 30.30303   UMAP
#> 4 430.4658 0.2722071  8 11  88 46 0.16 0.14 21.73913   UMAP
#> 5 406.8971 0.2544715  9 12 108 51 0.14 0.12 19.60784   UMAP
#> 6 380.7331 0.2388800 10 13 130 59 0.13 0.11 16.94915   UMAP
```
