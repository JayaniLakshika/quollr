% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2d.R
\name{tri_bin_centroids}
\alias{tri_bin_centroids}
\title{Triangulate bin centroids}
\usage{
tri_bin_centroids(centroids_data)
}
\arguments{
\item{centroids_data}{The tibble containing the all the bin centroids.}
}
\value{
A triangular object representing the triangulated bin centroids.
}
\description{
This function triangulates the bin centroids using the x and y coordinates
provided in the input data frame and returns the triangular object.
}
\examples{
all_centroids_df <- scurve_model_obj$hb_obj$centroids
counts_data <- scurve_model_obj$hb_obj$std_cts
umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
df_bin_centroids <- extract_hexbin_mean(data_hb = umap_with_hb_id,
counts_data = counts_data, centroids_data = all_centroids_df)
tri_bin_centroids(centroids_data = df_bin_centroids)

}
