# Summary with different number of bins for \`scurve_umap6\`

The \`scurve_umap_rmse6\` dataset contains error, RMSE, b2, b, m, a1,
a2, and mean density (d_bar) for different number of bins in x-axis
(b1).

## Usage

``` r
data(scurve_umap_rmse6)
```

## Format

\## \`scurve_umap_rmse6\` A data frame with 70 rows and 10 columns:

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
# Load the scurve_umap_rmse6 dataset
data(scurve_umap_rmse6)

# Display the first few rows of the dataset
head(scurve_umap_rmse6)
#>      Error      RMSE b1 b2   b  m   a1   a2      d_bar method
#> 1 3212.887 0.4245767  5  6  30 22 0.25 0.22 0.04480434  UMAP6
#> 2 2916.611 0.3822152  6  7  42 26 0.21 0.18 0.04619354  UMAP6
#> 3 2664.346 0.3463970  7  8  56 31 0.18 0.16 0.04422809  UMAP6
#> 4 2394.621 0.3083565  8  9  72 40 0.16 0.14 0.03469648  UMAP6
#> 5 2242.947 0.2826232  9 10  90 44 0.14 0.12 0.03628426  UMAP6
#> 6 2067.747 0.2604085 10 11 110 55 0.13 0.11 0.02867478  UMAP6
```
