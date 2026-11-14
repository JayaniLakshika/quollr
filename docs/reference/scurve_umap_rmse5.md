# Summary with different number of bins for \`scurve_umap5\`

The \`scurve_umap_rmse5\` dataset contains error, RMSE, b2, b, m, a1,
a2, and mean density (d_bar) for different number of bins in x-axis
(b1).

## Usage

``` r
data(scurve_umap_rmse5)
```

## Format

\## \`scurve_umap_rmse5\` A data frame with 70 rows and 10 columns:

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
# Load the scurve_umap_rmse5 dataset
data(scurve_umap_rmse5)

# Display the first few rows of the dataset
head(scurve_umap_rmse5)
#>      Error      RMSE b1 b2   b  m   a1   a2      d_bar method
#> 1 3197.875 0.4262868  5  6  30 22 0.25 0.22 0.04528909  UMAP5
#> 2 3070.927 0.4162682  6  7  42 28 0.21 0.18 0.04026108  UMAP5
#> 3 2605.361 0.3408828  7  8  56 36 0.18 0.16 0.03314387  UMAP5
#> 4 2453.070 0.3217673  8  9  72 44 0.16 0.14 0.02898502  UMAP5
#> 5 2210.033 0.2838858  9 10  90 52 0.14 0.12 0.02626499  UMAP5
#> 6 2025.989 0.2571960 10 11 110 63 0.13 0.11 0.02208671  UMAP5
```
