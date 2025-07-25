% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2d.R
\name{compute_std_counts}
\alias{compute_std_counts}
\title{Compute standardise counts in hexagons}
\usage{
compute_std_counts(scaled_nldr_hexid)
}
\arguments{
\item{scaled_nldr_hexid}{A tibble that contains the scaled embedding with hexagonal bin IDs.}
}
\value{
A tibble that contains hexagon IDs (\code{h}), bin counts (\code{n_h}),
and standardize counts (\code{w_h}).
}
\description{
This function computes the standardize number of points within each hexagon.
}
\examples{
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
compute_std_counts(scaled_nldr_hexid = umap_with_hb_id)

}
