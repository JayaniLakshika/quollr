---
title: "3. Algorithm for binning data"
output:
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{3. Algorithm for binning data}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



Here, we'll discuss the algorithm for binning data.


``` r
library(quollr)
library(ggplot2)
library(dplyr)
```

By passing the preprocessed 2-D embedding data and hexagonal grid configurations, you can obtain the hexagonal binning information like centroid coordinates, hexagonal polygon coordinates, the standardise counts within each hexagon etc.  


``` r
hb_obj <- hex_binning(
  nldr_scaled_obj = scurve_model_obj$nldr_scaled_obj, 
  b1 = 21, 
  q = 0.1)
```


``` r
## Data set with all possible centroids in the hexagonal grid
all_centroids_df <- hb_obj$centroids
glimpse(all_centroids_df)
#> Rows: 588
#> Columns: 3
#> $ h   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,…
#> $ c_x <dbl> -0.10000000, -0.04130351, 0.01739298, 0.07608947, 0.13478596, 0.19…
#> $ c_y <dbl> -0.11568014, -0.11568014, -0.11568014, -0.11568014, -0.11568014, -…

## Generate all coordinates of hexagons
hex_grid <- hb_obj$hex_poly
glimpse(hex_grid)
#> Rows: 3,528
#> Columns: 3
#> $ h <int> 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,…
#> $ x <dbl> -0.10000000, -0.12934824, -0.12934824, -0.10000000, -0.07065176, -0.…
#> $ y <dbl> -0.08179171, -0.09873593, -0.13262436, -0.14956858, -0.13262436, -0.…

## To obtain the standardise counts within hexbins
counts_df <- hb_obj$std_cts
df_bin_centroids <- merge_hexbin_centroids(centroids_data = all_centroids_df, 
                                           counts_data = counts_df)
```


``` r
ggplot(data = hex_grid, aes(x = x, y = y)) + 
  geom_polygon(fill = "white", color = "black", aes(group = h)) +
  geom_point(data = all_centroids_df, aes(x = c_x, y = c_y), color = "red") +
  coord_fixed()
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr3hexbin_files/figure-html/unnamed-chunk-4-1.png" alt="Grid with hexagon centroids."  />


``` r

ggplot(data = hex_grid, aes(x = x, y = y)) + 
  geom_polygon(fill = "white", color = "black", aes(group = h)) +
  geom_point(data = all_centroids_df, aes(x = c_x, y = c_y), color = "red") +
  geom_point(data = df_bin_centroids, aes(x = c_x, y = c_y), color = "purple") + 
  coord_fixed()
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr3hexbin_files/figure-html/unnamed-chunk-5-1.png" alt="Grid with hexagons colored empty bin centroid in red."  />


``` r
umap_scaled <- scurve_model_obj$nldr_scaled_obj$scaled_nldr

ggplot(data = hex_grid, aes(x = x, y = y)) + 
  geom_polygon(fill = "white", color = "black", aes(group = h)) +
  geom_point(data = umap_scaled, aes(x = emb1, y = emb2), color = "blue", alpha = 0.3) +
  coord_fixed()
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr3hexbin_files/figure-html/unnamed-chunk-6-1.png" alt="Grid with hexagons overlayed data."  />


``` r
hex_grid_with_counts <- left_join(hex_grid, counts_df, by = "h")

ggplot(data = hex_grid_with_counts, aes(x = x, y = y)) +
  geom_polygon(color = "black", aes(group = h, fill = w_h)) +
  scale_fill_viridis_c(direction = -1, na.value = "#ffffff") +
  coord_fixed()
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr3hexbin_files/figure-html/unnamed-chunk-7-1.png" alt="Grid with hexagons colored with density of points."  />

You can also use `geom_hexgrid` to visualise the hexagonal grid rather than `geom_polygon`.


``` r
ggplot(data = all_centroids_df, aes(x = c_x, y = c_y)) +
  geom_hexgrid() +
  coord_equal() +
  xlab("x") + ylab("y") +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7))
```

<img src="/Users/jpiy0001/Desktop/PhD Monash research files/quollr/vignettes/quollr3hexbin_files/figure-html/unnamed-chunk-8-1.png" alt="Grid with hexagons."  />
