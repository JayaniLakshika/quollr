# Object for S-curve dataset

The \`scurve_model_obj\` contains object of scaled umap embedding, x and
y limits, range of y; object of hexagonal binning information, object of
high-d model fitted to umap embedding for the training data, the
triangular object representing the triangulated bin centroids, a tibble
that contains the edge information, a tibble with edge distance
information.

## Usage

``` r
data(scurve_model_obj)
```

## Format

\## \`scurve_model_obj\` An object of five elements

- nldr_scaled_obj:

  A tibble contains scaled first and second columns of NLDR data, and
  numeric vectors representing the limits of the original NLDR data.

- hb_obj:

  A object that contains hexagonal binning information.

- model_highd:

  A tibble with high-dimensional model.

- model_2d:

  A tibble containing hexagonal bin centroids in 2-D

- trimesh_data:

  A tibble that contains the edge information.

## Source

This object is generated for illustrative purposes.

## Examples

``` r
# Load the scurve_model_obj
data(scurve_model_obj)
```
