% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{scurve_umap}
\alias{scurve_umap}
\title{UMAP embedding for S-curve dataset which with noise dimensions}
\format{
## `scurve_umap`
A data frame with 75 rows and 3 columns:
\describe{
  \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
  \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
  \item{ID}{Numeric, identifier for each data point.}
}
}
\source{
This dataset is generated for illustrative purposes.
}
\usage{
data(scurve_umap)
}
\description{
The `scurve_umap` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a three-dimensional S-curve with
added noise. Each data point is represented by two UMAP coordinates (UMAP1
and UMAP2) and an ID.
}
\examples{
# Load the scurve_umap dataset
data(scurve_umap)

# Display the first few rows of the dataset
head(scurve_umap)

}
\keyword{datasets}
