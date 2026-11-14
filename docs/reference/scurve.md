# S-curve dataset with noise dimensions

The \`scurve\` dataset contains a 3-dimensional S-curve with added noise
dimensions. Each data point is represented by seven dimensions (x1 to
x7) and an ID.

## Usage

``` r
data(scurve)
```

## Format

A data frame with 5000 rows and 8 columns:

- ID:

  Identification number

- x1, x2, x3, x4, x5, x6, x7:

  High-dimensional coordinates

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the scurve dataset
data(scurve)

# Display the first few rows of the dataset
head(scurve)
#> # A tibble: 6 × 8
#>        x1    x2       x3       x4       x5      x6        x7    ID
#>     <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl> <int>
#> 1 -0.120  0.819 -1.99     0.0114   0.00351  0.0334  0.00638      1
#> 2 -0.0492 0.166  0.00121  0.0115  -0.0166  -0.0297  0.00509      2
#> 3 -0.774  0.651  0.367   -0.0172   0.00600  0.0211  0.00303      3
#> 4 -0.606  0.952 -1.80     0.0157  -0.00978 -0.0590 -0.00754      4
#> 5 -0.478  1.10  -1.88    -0.00423  0.00495 -0.0482 -0.00982      5
#> 6  0.818  1.78  -1.58     0.0124   0.0198   0.0560 -0.000730     6
```
