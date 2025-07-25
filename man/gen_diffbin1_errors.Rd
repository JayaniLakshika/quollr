% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction.R
\name{gen_diffbin1_errors}
\alias{gen_diffbin1_errors}
\title{Generate erros and MSE for different bin widths}
\usage{
gen_diffbin1_errors(highd_data, nldr_data, benchmark_highdens = 1)
}
\arguments{
\item{highd_data}{A tibble that contains the high-dimensional data with a unique identifier.}

\item{nldr_data}{A tibble that contains the embedding with a unique identifier.}

\item{benchmark_highdens}{(default: 1) A numeric value using to filter high-density hexagons.}
}
\value{
A tibble containing the augmented data with predictions,
error metrics, and absolute error metrics.
}
\description{
This function augments a dataset with predictions and error metrics obtained
from a nonlinear dimension reduction (NLDR) model.
}
\examples{
scurve_sample <- scurve |> head(100)
scurve_umap_sample <- scurve_umap |> head(100)
gen_diffbin1_errors(highd_data = scurve_sample, nldr_data = scurve_umap_sample)

}
