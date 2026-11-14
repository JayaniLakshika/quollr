# quollr: Visualising How Nonlinear Dimension Reduction Warps Your Data

quollr helps you assess nonlinear dimension reduction (NLDR) layouts by
constructing a 2-D hexagonal-bin wireframe model and lifting it into
high-dimensional space for visual inspection. This makes it possible to
determine which NLDR method and hyper-parameter choices produce the most
faithful representation of high-dimensional data.

## Details

The main workflow is:

1.  Fit the model with
    [`fit_highd_model`](https://jayanilakshika.github.io/quollr/reference/fit_highd_model.md)

2.  Combine data and model with
    [`comb_data_model`](https://jayanilakshika.github.io/quollr/reference/comb_data_model.md)

3.  Inspect in high dimensions with
    [`show_langevitour`](https://jayanilakshika.github.io/quollr/reference/show_langevitour.md)

4.  Evaluate fit with
    [`glance`](https://jayanilakshika.github.io/quollr/reference/glance.md)
    and
    [`augment`](https://jayanilakshika.github.io/quollr/reference/augment.md)

## See also

Useful links:

- <https://jayanilakshika.github.io/quollr/>

- <https://arxiv.org/abs/2506.22051>

## Author

**Maintainer**: Jayani P. Gamage <jayanilakshika76@gmail.com>
([ORCID](https://orcid.org/0000-0002-6265-6481))

Authors:

- Dianne Cook <dicook@monash.edu>
  ([ORCID](https://orcid.org/0000-0002-3813-7155))

- Paul Harrison <paul.harrison@monash.edu>
  ([ORCID](https://orcid.org/0000-0002-3980-268X))

- Michael Lydeamore <michael.lydeamore@monash.edu>
  ([ORCID](https://orcid.org/0000-0001-6515-827X))

- Thiyanga S. Talagala <ttalagala@sjp.ac.lk>
  ([ORCID](https://orcid.org/0000-0002-0656-9789))

## Examples

``` r
# Fit the complete model pipeline
model <- fit_highd_model(
  highd_data = scurve,
  nldr_data  = scurve_umap,
  b1         = 15,
  q          = 0.1,
  hd_thresh  = 0
)
#> ✔ Model generated successfully!!!

# Combine data and model for visualisation
df <- comb_data_model(
  highd_data  = scurve,
  model_highd = model$model_highd,
  model_2d    = model$model_2d
)

# Evaluate goodness of fit
glance(x = model, highd_data = scurve)
#> # A tibble: 1 × 2
#>   Error   HBE
#>   <dbl> <dbl>
#> 1  265. 0.157

# Inspect the lifted model in high dimensions (interactive only)
if (FALSE) { # \dontrun{
  show_langevitour(
    point_data = df,
    edge_data  = model$trimesh_data
  )
} # }
```
