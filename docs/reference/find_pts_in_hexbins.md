# Find Points in Hexagonal Bins

This function maps points to their corresponding hexagonal bins based on
the provided data frames.

## Usage

``` r
find_pts_in_hexbins(nldr_data_with_hb_id)
```

## Arguments

- nldr_data_with_hb_id:

  A data frame with 2D embedding data, ID and hexagonal bin IDs.

## Value

A data frame with hexagonal bin IDs and the corresponding points.

## Examples

``` r
num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
                                        x = "UMAP1", hex_size = NA, buffer_x = NA)
#> Hex size is set to 0.2.
#> Buffer along the x-axis is set to 0.519615242270663.
num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
                                        y = "UMAP2", hex_size = NA, buffer_y = NA)
#> Hex size is set to 0.2.
#> Buffer along the y-axis is set to 0.45.
centroid_list <- generate_full_grid_centroids(nldr_df = s_curve_noise_umap_scaled,
                                             x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
                                             num_bins_y = num_bins_y, x_start = NA,
                                             y_start = NA, buffer_x = NA,
                                             buffer_y = NA, hex_size = NA)
#> Hex size is set to 0.2.
#> x_start is set to -0.173205080756888.
#> y_start is set to -0.15.
all_centroids_df <- as.data.frame(do.call(cbind, centroid_list))
s_curve_noise_umap_scaled_rm_id <- s_curve_noise_umap_scaled |> dplyr::select(-ID)
nldr_with_hb_id_list <- assign_data(nldr_df = s_curve_noise_umap_scaled_rm_id,
centroid_df = all_centroids_df)
umap_with_hb_id <- as.data.frame(do.call(cbind, nldr_with_hb_id_list))
umap_with_hb_id <- umap_with_hb_id |> dplyr::mutate(ID = s_curve_noise_umap_scaled$ID)
find_pts_in_hexbins(nldr_data_with_hb_id = umap_with_hb_id)
#> $hexID
#>  [1]  6 18 24  2 12 34 28 13  7 29
#> 
#> $pts_list
#> $pts_list$`Points in hexID: 6`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 18`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 24`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 2`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 12`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 34`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 28`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 13`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 7`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> $pts_list$`Points in hexID: 29`
#>  [1]   1   2   3   4   6   7   8   9  11  12  14  15  16  17  19  20  21  22  23
#> [20]  24  25  26  31  33  34  35  37  38  39  40  41  42  43  44  45  46  47  51
#> [39]  52  54  55  56  57  59  60  62  63  64  65  66  67  69  70  71  72  73  74
#> [58]  75  76  77  78  79  80  81  84  87  89  91  93  94  95  96  97  99 100
#> 
#> 
```
