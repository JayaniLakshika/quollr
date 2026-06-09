---
title: "2. Data preprocessing"
output:
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{2. Data preprocessing}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



Here, we'll walk through the process of preprocessing 2-D embedding data to obtain regular hexagons.


``` r
library(quollr)
library(dplyr)
```

First, you'll need 2-D embedding data generated for your high-dimensional data. For our example, we'll use a $3\text{-}D$ S-curve dataset with four additional noise dimensions (`scurve`). We've used UMAP as our non-linear dimension reduction method (NLDR) to generate embeddings for the `scurve` data.


``` r
scaled_umap <- gen_scaled_data(nldr_data = scurve_umap)

glimpse(scaled_umap)
#> List of 3
#>  $ scaled_nldr: tibble [1,000 × 3] (S3: tbl_df/tbl/data.frame)
#>   ..$ emb1: num [1:1000] 0.277 0.697 0.779 0.173 0.218 ...
#>   ..$ emb2: num [1:1000] 0.913 0.538 0.399 0.953 0.983 ...
#>   ..$ ID  : int [1:1000] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ lim1       : num [1:2] -9.15 8.55
#>  $ lim2       : num [1:2] -10.4 10.1
```

The function `gen_scaled_data()` standardises the 2-D embedding and rescales it so that hexagons generated during visualisation or analysis will be regular. 
