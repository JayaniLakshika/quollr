# Generate a design to layout 2-D representations

This function generates a design which can be passed to
\`plot_layout()\` to arrange 2-D layouts.

## Usage

``` r
gen_design(n_right, ncol_right = 2)
```

## Arguments

- n_right:

  The number of plots in right side.

- ncol_right:

  The number of columns in right side.

## Value

A patchwork area object.

## Examples

``` r
gen_design(n_right = 8, ncol_right = 2)
#> 9 patch areas, spanning 3 columns and 4 rows
#> 
#>     t l b r
#> 1:  1 1 4 1
#> 2:  1 2 1 2
#> 3:  1 3 1 3
#> 4:  2 2 2 2
#> 5:  2 3 2 3
#> 6:  3 2 3 2
#> 7:  3 3 3 3
#> 8:  4 2 4 2
#> 9:  4 3 4 3
```
