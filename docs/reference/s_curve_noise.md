# S-curve dataset with noise dimensions

The \`s_curve_noise\` dataset contains a 3-dimensional S-curve with
added noise dimensions. Each data point is represented by seven
dimensions (x1 to x7) and an ID.

## Usage

``` r
data(s_curve_noise)
```

## Format

A data frame with 100 rows and 8 columns:

- ID:

  Identification number

- x1, x2, x3, x4, x5, x6, x7:

  High-dimensional coordinates

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the s_curve_noise dataset
data(s_curve_noise)

# Display the first few rows of the dataset
head(s_curve_noise)
#> # A tibble: 6 × 8
#>        x1     x2       x3       x4       x5      x6        x7    ID
#>     <dbl>  <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl> <int>
#> 1 -0.120  1.64   -1.99     0.0104   0.0125   0.0923 -0.00128      1
#> 2 -0.0492 1.51    0.00121 -0.0177   0.00726 -0.0362 -0.00535      2
#> 3 -0.774  1.30    0.367   -0.00173  0.0156  -0.0962  0.00335      3
#> 4 -0.606  0.246  -1.80    -0.00897 -0.0187  -0.0716  0.00126      4
#> 5 -0.478  0.0177 -1.88     0.00848  0.00533  0.0998  0.000677     5
#> 6  0.818  0.927  -1.58    -0.00318 -0.00980  0.0989  0.00696      6
```
