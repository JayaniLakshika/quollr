# S-curve dataset with noise dimensions for test

The \`s_curve_noise_test\` dataset contains test data with dimensions
x1, x2, x3, x4, x5, x6, and x7. Each data point is identified by an ID.

## Usage

``` r
data(s_curve_noise_test)
```

## Format

A data frame with 25 rows and 8 columns:

- ID:

  Identification number

- x1, x2, x3, x4, x5, x6, x7:

  High-dimensional coordinates

## Source

This dataset is generated for training purposes.

## Examples

``` r
# Load the s_curve_noise_test dataset
data(s_curve_noise_test)

# Display the first few rows of the dataset
head(s_curve_noise_test)
#> # A tibble: 6 × 8
#>        x1    x2       x3       x4       x5      x6        x7    ID
#>     <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl>     <dbl> <int>
#> 1 -0.606  0.246 -1.80    -0.00897 -0.0187  -0.0716  0.00126      4
#> 2 -0.0691 1.59   0.00239  0.0127  -0.0130   0.0396 -0.000185     8
#> 3  0.513  1.02   1.86     0.0141  -0.0149   0.0619 -0.00309     13
#> 4  0.869  0.576 -0.505   -0.0196   0.00169 -0.0197  0.00597     14
#> 5 -0.737  1.94  -1.68     0.00601 -0.0113   0.0301 -0.00988     17
#> 6 -0.795  0.488  1.61    -0.0126   0.0131  -0.0956 -0.00283     20
```
