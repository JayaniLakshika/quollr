# Package index

## Package overview

Package-level documentation.

- [`quollr`](https://jayanilakshika.github.io/quollr/reference/quollr.md)
  [`quollr-package`](https://jayanilakshika.github.io/quollr/reference/quollr.md)
  : quollr: Visualising How Nonlinear Dimension Reduction Warps Your
  Data

## Data

The example data sets.

- [`scurve`](https://jayanilakshika.github.io/quollr/reference/scurve.md)
  : S-curve dataset with noise dimensions
- [`scurve_model_obj`](https://jayanilakshika.github.io/quollr/reference/scurve_model_obj.md)
  : Object for S-curve dataset
- [`scurve_plts`](https://jayanilakshika.github.io/quollr/reference/scurve_plts.md)
  : List of plots
- [`scurve_umap`](https://jayanilakshika.github.io/quollr/reference/scurve_umap.md)
  : UMAP embedding for \`scurve\` with n_neighbors = 15 and min_dist =
  0.1
- [`scurve_umap_predict`](https://jayanilakshika.github.io/quollr/reference/scurve_umap_predict.md)
  : Predicted UMAP embedding for \`scurve\` data
- [`scurve_umap2`](https://jayanilakshika.github.io/quollr/reference/scurve_umap2.md)
  : UMAP embedding for \`scurve\` with n_neighbors = 10 and min_dist =
  0.4
- [`scurve_umap3`](https://jayanilakshika.github.io/quollr/reference/scurve_umap3.md)
  : UMAP embedding for \`scurve\` with n_neighbors = 62 and min_dist =
  0.1
- [`scurve_umap4`](https://jayanilakshika.github.io/quollr/reference/scurve_umap4.md)
  : UMAP embedding for \`scurve\` with n_neighbors = 30 and min_dist =
  0.5
- [`scurve_umap_rmse`](https://jayanilakshika.github.io/quollr/reference/scurve_umap_rmse.md)
  : Summary with different number of bins for \`scurve_umap\`
- [`scurve_umap_rmse2`](https://jayanilakshika.github.io/quollr/reference/scurve_umap_rmse2.md)
  : Summary with different number of bins for \`scurve_umap2\`
- [`scurve_umap_rmse3`](https://jayanilakshika.github.io/quollr/reference/scurve_umap_rmse3.md)
  : Summary with different number of bins for \`scurve_umap3\`
- [`scurve_umap_rmse4`](https://jayanilakshika.github.io/quollr/reference/scurve_umap_rmse4.md)
  : Summary with different number of bins for \`scurve_umap4\`

## Data preprocessing

These are for data preprocessing.

- [`gen_scaled_data()`](https://jayanilakshika.github.io/quollr/reference/gen_scaled_data.md)
  : Scaling the NLDR data

## Hexagonal binning

These are for hexagonal binning.

- [`calc_bins_y()`](https://jayanilakshika.github.io/quollr/reference/calc_bins_y.md)
  : Calculate the effective number of bins along x-axis and y-axis
- [`gen_centroids()`](https://jayanilakshika.github.io/quollr/reference/gen_centroids.md)
  : Generate centroid coordinate
- [`gen_hex_coord()`](https://jayanilakshika.github.io/quollr/reference/gen_hex_coord.md)
  : Generate hexagonal polygon coordinates
- [`assign_data()`](https://jayanilakshika.github.io/quollr/reference/assign_data.md)
  : Assign data to hexagons
- [`compute_std_counts()`](https://jayanilakshika.github.io/quollr/reference/compute_std_counts.md)
  : Compute standardise counts in hexagons
- [`hex_binning()`](https://jayanilakshika.github.io/quollr/reference/hex_binning.md)
  : Hexagonal binning

## 2-D model construction

These are to construct the 2-D model.

- [`merge_hexbin_centroids()`](https://jayanilakshika.github.io/quollr/reference/merge_hexbin_centroids.md)
  : Extract hexagonal bin centroids coordinates and the corresponding
  standardise counts.
- [`merge_hexbin_mean()`](https://jayanilakshika.github.io/quollr/reference/merge_hexbin_mean.md)
  : Extract hexagonal bin mean coordinates and the corresponding
  standardize counts.
- [`tri_bin_centroids()`](https://jayanilakshika.github.io/quollr/reference/tri_bin_centroids.md)
  : Triangulate bin centroids
- [`gen_edges()`](https://jayanilakshika.github.io/quollr/reference/gen_edges.md)
  : Generate edge information
- [`update_trimesh_index()`](https://jayanilakshika.github.io/quollr/reference/update_trimesh_index.md)
  : Update from and to values in trimesh data

## Parameters

These are to compute default parameter values.

- [`compute_mean_density_hex()`](https://jayanilakshika.github.io/quollr/reference/compute_mean_density_hex.md)
  : Compute mean density of hexagonal bins
- [`find_low_dens_hex()`](https://jayanilakshika.github.io/quollr/reference/find_low_dens_hex.md)
  : Find low-density Hexagons

## Lift to high dimensions

The function is to lift the constructed 2-D model into high-dimensions.

- [`avg_highd_data()`](https://jayanilakshika.github.io/quollr/reference/avg_highd_data.md)
  : Create a tibble with averaged high-dimensional data

## Fit the model

The function is to fit the model.

- [`fit_highd_model()`](https://jayanilakshika.github.io/quollr/reference/fit_highd_model.md)
  : Construct the 2-D model and lift into high-dimensions

## Model summaries

These are to obtain model summaries.

- [`glance()`](https://jayanilakshika.github.io/quollr/reference/glance.md)
  : S3 generic for glance
- [`glance(`*`<highd_vis_model>`*`)`](https://jayanilakshika.github.io/quollr/reference/glance.highd_vis_model.md)
  : Generate evaluation metrics for a hex_model object
- [`predict_emb()`](https://jayanilakshika.github.io/quollr/reference/predict_emb.md)
  : Predict 2-D embeddings
- [`augment()`](https://jayanilakshika.github.io/quollr/reference/augment.md)
  : S3 generic for augment
- [`augment(`*`<highd_vis_model>`*`)`](https://jayanilakshika.github.io/quollr/reference/augment.highd_vis_model.md)
  : Augment Data with Predictions and Error Metrics for NLDR Models
- [`gen_diffbin1_errors()`](https://jayanilakshika.github.io/quollr/reference/gen_diffbin1_errors.md)
  : Generate erros and MSE for different bin widths
- [`plot_hbe_layouts()`](https://jayanilakshika.github.io/quollr/reference/plot_hbe_layouts.md)
  : Arrange HBE plot and 2-D layouts

## 2-D visualisation

These are to visualise the model in 2-D.

- [`GeomHexgrid`](https://jayanilakshika.github.io/quollr/reference/GeomHexgrid.md)
  : GeomHexgrid: A Custom ggplot2 Geom for Hexagonal Grid
- [`stat_hexgrid()`](https://jayanilakshika.github.io/quollr/reference/stat_hexgrid.md)
  : stat_hexgrid Custom Stat for hexagonal grid plot
- [`geom_hexgrid()`](https://jayanilakshika.github.io/quollr/reference/geom_hexgrid.md)
  : Create a hexgrid plot
- [`GeomTrimesh`](https://jayanilakshika.github.io/quollr/reference/GeomTrimesh.md)
  : GeomTrimesh: A Custom ggplot2 Geom for Triangular Meshes
- [`stat_trimesh()`](https://jayanilakshika.github.io/quollr/reference/stat_trimesh.md)
  : stat_trimesh Custom Stat for trimesh plot
- [`geom_trimesh()`](https://jayanilakshika.github.io/quollr/reference/geom_trimesh.md)
  : Create a trimesh plot

## High-dimensional visualisation

These are to visualise the model in high-dimensional space.

- [`comb_data_model()`](https://jayanilakshika.github.io/quollr/reference/comb_data_model.md)
  : Create a tibble with averaged high-dimensional data and
  high-dimensional data
- [`show_langevitour()`](https://jayanilakshika.github.io/quollr/reference/show_langevitour.md)
  : Visualise the model overlaid on high-dimensional data

## Projection

These are to generate 2-D projections from tour.

- [`gen_axes()`](https://jayanilakshika.github.io/quollr/reference/gen_axes.md)
  : Generate Axes for Projection
- [`get_projection()`](https://jayanilakshika.github.io/quollr/reference/get_projection.md)
  : Compute Projection for High-Dimensional Data
- [`plot_proj()`](https://jayanilakshika.github.io/quollr/reference/plot_proj.md)
  : Plot Projected Data with Axes and Circles

## Link plots

These are to diagnoise with interactivity.

- [`comb_all_data_model()`](https://jayanilakshika.github.io/quollr/reference/comb_all_data_model.md)
  : Create a tibble with averaged high-dimensional data and
  high-dimensional data, non-linear dimension reduction data
- [`show_link_plots()`](https://jayanilakshika.github.io/quollr/reference/show_link_plots.md)
  : Visualise the model overlaid on high-dimensional data along with 2-D
  wireframe model.
- [`comb_all_data_model_error()`](https://jayanilakshika.github.io/quollr/reference/comb_all_data_model_error.md)
  : Create a tibble with averaged high-dimensional data and
  high-dimensional data, non-linear dimension reduction data, model
  error data
- [`show_error_link_plots()`](https://jayanilakshika.github.io/quollr/reference/show_error_link_plots.md)
  : Visualise the model overlaid on high-dimensional data along with 2-D
  wireframe model and error.

## Additional

These are additional functionalities.

- [`group_hex_pts()`](https://jayanilakshika.github.io/quollr/reference/group_hex_pts.md)
  : Grouped points in each hexagon
- [`find_non_empty_bins()`](https://jayanilakshika.github.io/quollr/reference/find_non_empty_bins.md)
  : Find the number of bins required to achieve required number of
  non-empty bins.
- [`gen_design()`](https://jayanilakshika.github.io/quollr/reference/gen_design.md)
  : Generate a design to layout 2-D representations
- [`quad()`](https://jayanilakshika.github.io/quollr/reference/quad.md)
  : Solve Quadratic Equation for Positive Real Roots
- [`calc_2d_dist()`](https://jayanilakshika.github.io/quollr/reference/calc_2d_dist.md)
  : Calculate 2-D Euclidean distances between vertices
