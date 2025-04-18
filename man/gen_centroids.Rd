% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_2d.R
\name{gen_centroids}
\alias{gen_centroids}
\title{Generate centroid coordinate}
\usage{
gen_centroids(bin1 = 2, r2, q = 0.1)
}
\arguments{
\item{bin1}{Number of bins along the x axis.}

\item{r2}{The ratio of the ranges of the original embedding components.}

\item{q}{The buffer amount as proportion of data range.}
}
\value{
A tibble contains hexIDs, x and y coordinates (hexID, c_x, c_y respectively)
of all hexagon bin centroids.
}
\description{
This function generates all possible centroids in the hexagonal grid.
}
\examples{
scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
lim1 <- scurve_umap_scaled_obj$lim1
lim2 <- scurve_umap_scaled_obj$lim2
r2 <- diff(lim2)/diff(lim1)
gen_centroids(bin1 = 4, r2 = r2, q = 0.1)

}
