# Visualize a specific projection of langevitour

This function visualize a specific projection of langevitour.

## Usage

``` r
gen_proj_langevitour(points_df, projection, edge_df)
```

## Arguments

- points_df:

  The tibble that contains the model and data.

- projection:

  The tibble of the projection.

- edge_df:

  The tibble that contains the edge information (from, to).

## Value

A ggplot object with the specific projection of langevitour.

## Examples

``` r
umap_data_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
umap_data_with_hb_id)
df_bin_centroids <- s_curve_obj$s_curve_umap_model_distance_df$df_bin_centroids
#> Warning: Unknown or uninitialised column: `df_bin_centroids`.
df_bin <- s_curve_obj$s_curve_umap_model_distance_df$df_bin
#> Warning: Unknown or uninitialised column: `df_bin`.
distance_df <- s_curve_obj$distance_df
### Define type column
df <- df_all |>
  dplyr::select(tidyselect::starts_with("x")) |>
  dplyr::mutate(type = "data") ## original dataset

df_b <- df_bin |>
  dplyr::filter(hb_id %in% df_bin_centroids$hexID) |>
  dplyr::mutate(type = "model") ## Data with summarized mean
#> Error in UseMethod("filter"): no applicable method for 'filter' applied to an object of class "NULL"

## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
df_b <- df_b[match(df_bin_centroids$hexID, df_b$hb_id),] |>
  dplyr::select(-hb_id)
#> Error: object 'df_b' not found

df_exe <- dplyr::bind_rows(df_b, df)
#> Error: object 'df_b' not found
benchmark <- 0.663

## Set the maximum difference as the criteria
distance_df_small_edges <- distance_df |>
  dplyr::filter(distance < benchmark) |>
  dplyr::select(-distance)
#> Error in UseMethod("filter"): no applicable method for 'filter' applied to an object of class "NULL"

projection_df <- cbind(
c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))


gen_proj_langevitour(points_df = df_exe, projection = projection_df,
edge_df = distance_df_small_edges)
#> Error: object 'df_exe' not found
```
