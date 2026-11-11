# S-curve dataset with noise dimensions for training

The \`s_curve_noise_training\` dataset contains training data with
dimensions x1, x2, x3, x4, x5, x6, and x7. Each data point is identified
by an ID.

## Usage

``` r
data(s_curve_noise_training)
```

## Format

A data frame with 75 rows and 8 columns:

- ID:

  Identification number

- x1, x2, x3, x4, x5, x6, x7:

  High-dimensional coordinates

## Source

This dataset is generated for training purposes.

## Examples

``` r
# Load the s_curve_noise_training dataset
data(s_curve_noise_training)

# Display the first few rows of the dataset
head(s_curve_noise_training)
#> # A tibble: 6 × 8
#>        x1     x2       x3       x4       x5      x6        x7    ID
#>     <dbl>  <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl> <int>
#> 1 -0.120  1.64   -1.99     0.0104   0.0125   0.0923 -0.00128      1
#> 2 -0.0492 1.51    0.00121 -0.0177   0.00726 -0.0362 -0.00535      2
#> 3 -0.774  1.30    0.367   -0.00173  0.0156  -0.0962  0.00335      3
#> 4 -0.478  0.0177 -1.88     0.00848  0.00533  0.0998  0.000677     5
#> 5  0.818  0.927  -1.58    -0.00318 -0.00980  0.0989  0.00696      6
#> 6  0.910  1.40    1.42     0.00699 -0.0182  -0.0710  0.00966      7
```
