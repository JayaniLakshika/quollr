% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{find_low_dens_hex}
\alias{find_low_dens_hex}
\title{Find low-density Hexagons}
\usage{
find_low_dens_hex(centroids_data, b1 = 4, benchmark_mean_dens = 0.05)
}
\arguments{
\item{b1}{Number of bins along the x-axis for hexagon binning.}

\item{df_bin_centroids_all}{The tibble that contains all hexagonal bin centroids.}

\item{df_bin_centroids_low}{The tibble that contains identified low-density hexagonal bin centroids.}
}
\value{
A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
}
\description{
This function identifies hexagons with low density based on the mean density
of their neighboring hexagons.
}
\examples{
find_low_dens_hex(centroids_data = scurve_model_obj$model_2d, b1 = 4,
benchmark_mean_dens = 0.05)

}
