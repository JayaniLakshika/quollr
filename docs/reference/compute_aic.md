# Compute the Akaike Information Criterion (AIC) for a given model.

Compute the Akaike Information Criterion (AIC) for a given model.

## Usage

``` r
compute_aic(p, mse, num_bins, num_obs)
```

## Arguments

- p:

  Number of dimensions of the data set.

- mse:

  Mean squared error (MSE) of the model.

- num_bins:

  Total number of bins without empty bins used in the model.

- num_obs:

  Total number of observations in the training or test set.

## Value

The AIC value for the specified model.

## Examples

``` r
# Example usage of compute_aic function
p <- 5
mse <- 1500
num_bins <- 10
num_obs <- 100
aic_value <- compute_aic(p, mse, num_bins, num_obs)
cat("AIC Value:", aic_value, "\n")
#> AIC Value: 3756.61 
```
