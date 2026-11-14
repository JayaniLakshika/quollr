# Summary with different number of bins for \`scurve_umap4\`

The \`scurve_umap_rmse4\` dataset contains error, RMSE, b2, b, m, a1,
a2, and mean density (d_bar) for different number of bins in x-axis
(b1).

## Usage

``` r
data(scurve_umap_rmse4)
```

## Format

\## \`scurve_umap_rmse4\` A data frame with 70 rows and 10 columns:

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
# Load the scurve_umap_rmse4 dataset
data(scurve_umap_rmse4)

# Display the first few rows of the dataset
head(scurve_umap_rmse4)
#>      Error       HBE b1 b2   b  m   a1   a2    n_bar method
#> 1 647.4267 0.4259840  5  7  35 21 0.30 0.26 47.61905  UMAP4
#> 2 543.8493 0.3550077  6  9  54 29 0.22 0.19 34.48276  UMAP4
#> 3 494.4202 0.3171872  7 10  70 34 0.20 0.17 29.41176  UMAP4
#> 4 429.9829 0.2721302  8 12  96 46 0.16 0.14 21.73913  UMAP4
#> 5 399.9027 0.2512117  9 13 117 53 0.15 0.13 18.86792  UMAP4
#> 6 353.1208 0.2208283 10 15 150 66 0.13 0.11 15.15152  UMAP4
```
