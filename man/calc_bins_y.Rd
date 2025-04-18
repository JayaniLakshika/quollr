% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_util.R
\name{calc_bins_y}
\alias{calc_bins_y}
\title{Calculate the effective number of bins along x-axis and y-axis}
\usage{
calc_bins_y(bin1 = 4, r2, q = 0.1)
}
\arguments{
\item{bin1}{Number of bins along the x axis.}

\item{r2}{The ratio of the ranges of the original embedding components.}

\item{q}{The buffer amount as proportion of data range.}
}
\value{
A list of numeric values that represents the effective number of
bins along the y axis, height and, width of the hexagon.
}
\description{
This function calculates the effective number of bins along the x and y axes
of a hexagonal grid.
}
\examples{
scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
lim1 <- scurve_umap_scaled_obj$lim1
lim2 <- scurve_umap_scaled_obj$lim2
r2 <- diff(lim2)/diff(lim1)
calc_bins_y(bin1 = 4, r2 = r2, q = 0.1)

}
