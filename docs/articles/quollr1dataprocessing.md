# 1. Data preprocessing

Here, we’ll walk through the process of preprocessing 2-D embedding data
to obtain regular hexagons.

``` r

library(quollr)
library(dplyr)
```

First, you’ll need 2-D embedding data generated for your
high-dimensional data. For our example, we’ll use a $`3\text{-}D`$
S-curve dataset with four additional noise dimensions (`scurve`). We’ve
used UMAP as our non-linear dimension reduction method (NLDR) to
generate embeddings for the `scurve` data.

``` r

scaled_umap <- gen_scaled_data(nldr_data = scurve_umap)

glimpse(scaled_umap)
#> List of 3
#>  $ scaled_nldr: tibble [5,000 × 3] (S3: tbl_df/tbl/data.frame)
#>   ..$ emb1: num [1:5000] 0.707 0.231 0.232 0.79 0.761 ...
#>   ..$ emb2: num [1:5000] 0.839 0.401 0.215 0.564 0.551 ...
#>   ..$ ID  : int [1:5000] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ lim1       : num [1:2] -14.4 13.3
#>  $ lim2       : num [1:2] -12.4 12.3
```

The function
[`gen_scaled_data()`](https://jayanilakshika.github.io/quollr/reference/gen_scaled_data.md)
standardises the 2-D embedding and rescales it so that hexagons
generated during visualisation or analysis will be regular.
