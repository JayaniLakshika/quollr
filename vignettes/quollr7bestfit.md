---
title: "7. Selecting the best fit"
output:
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{7. Selecting the best fit}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
library(quollr)
library(dplyr)
library(ggplot2)
library(patchwork)

set.seed(20240110)
```

This illustrates how to compare multiple NLDR layouts and select the most appropriate one. The evaluation is typically based on hexbin error (HBE), allowing you to assess the quality of different binning configurations or modeling strategies.

## Step 1: Generate design for 2-D NLDR layouts

We begin by specifying a layout design for arranging the plots. The design defines the arrangement of 2-D NLDR layouts.


``` r
design <- gen_design(n_right = 4, ncol_right = 2)
```

## Step 2: Visualising HBE across configurations

To visually compare multiple 2-D NLDR layouts, we use the `plot_hbe_layouts()` function. This function plots HBE values against different bin width configurations along with 2-D NLDR layouts, making it easier to identify the best-performing model visually.


``` r

plot_hbe_layouts(plots = scurve_plts, design = design)
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr7bestfit_files/figure-html/unnamed-chunk-3-1.png" alt="HBE Vs binwidths." width="100%" />

