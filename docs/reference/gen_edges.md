# Generate edge information

This function generates edge information from a given triangular object,
including the coordinates of the vertices and the from-to relationships
between the vertices.

## Usage

``` r
gen_edges(tri_object, a1)
```

## Arguments

- tri_object:

  The triangular object from which to generate edge information.

- a1:

  A numeric value for bin width.

## Value

A tibble that contains the edge information, including the from-to
relationships and the corresponding x and y coordinates.

## Examples

``` r
all_centroids_df <- scurve_model_obj$hb_obj$centroids
counts_data <- scurve_model_obj$hb_obj$std_cts
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
df_bin_centroids <- merge_hexbin_centroids(counts_data = counts_data,
centroids_data = all_centroids_df)
tr1_object <- tri_bin_centroids(centroids_data = df_bin_centroids)
gen_edges(tri_object = tr1_object, a1 = scurve_model_obj$hb_obj$a1)
#> # A tibble: 827 × 8
#>     from    to  x_from  y_from    x_to    y_to from_count to_count
#>    <int> <int>   <dbl>   <dbl>   <dbl>   <dbl>      <dbl>    <dbl>
#>  1     1     2 -0.1    -0.116  -0.0166 -0.116           0        0
#>  2    16    17 -0.0583 -0.0434  0.0251 -0.0434          0        0
#>  3    16    32 -0.0583 -0.0434 -0.0166  0.0288          0        0
#>  4     3    17  0.0668 -0.116   0.0251 -0.0434          0        0
#>  5    32    33 -0.0166  0.0288  0.0668  0.0288          0        0
#>  6    17    18  0.0251 -0.0434  0.109  -0.0434          0        0
#>  7    31    46 -0.1     0.0288 -0.0583  0.101           0        0
#>  8    32    47 -0.0166  0.0288  0.0251  0.101           0        0
#>  9    18    19  0.109  -0.0434  0.192  -0.0434          0        0
#> 10    33    34  0.0668  0.0288  0.150   0.0288          0        1
#> # ℹ 817 more rows
```
