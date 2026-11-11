# Solve Quadratic Equation for Positive Real Roots

This function solves a quadratic equation of the form \`ax^2 + bx + c =
0\` and returns only the positive real roots. It handles complex
intermediate calculations and returns real numbers if the roots are
real.

## Usage

``` r
quad(a = 3, b = 2, c = -1)
```

## Arguments

- a:

  The coefficient of the x^2 term. Can be complex.

- b:

  The coefficient of the x term.

- c:

  The constant term.

## Value

A numeric vector containing the positive real root(s) of the quadratic
equation. Returns \`numeric(0)\` if no positive real roots are found.
Returns a single value if both positive roots are identical.

## Examples

``` r

# Example 1: With specific coefficients
quad(a = 1, b = -3, c = 2) # x^2 - 3x + 2 = 0
#> [1] 2 1
```
