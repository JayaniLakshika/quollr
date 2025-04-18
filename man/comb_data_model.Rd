% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_highd.R
\name{comb_data_model}
\alias{comb_data_model}
\title{Create a dataframe with averaged high-dimensional data and high-dimensional data}
\usage{
comb_data_model(highd_data, model_highd, model_2d)
}
\arguments{
\item{highd_data}{A tibble that contains the high-dimensional data.}

\item{model_highd}{A tibble that contains the high-dimensional coordinates of bin centroids.}

\item{model_2d}{The dataset with hexagonal bin centroids.}
}
\value{
A tibble with the average values of the high-dimensional data within
each hexagonal bin and high-dimensional data.
}
\description{
This function combine the average values of high-dimensional data within each
hexagonal bin and high-dimensional data.
}
\examples{
df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
df_bin <- s_curve_obj$s_curve_umap_model_obj$df_bin
comb_data_mode(highd_data = s_curve_noise_training, model_highd = df_bin,
model_2d = df_bin_centroids)

}
