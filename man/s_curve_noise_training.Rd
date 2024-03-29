% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{s_curve_noise_training}
\alias{s_curve_noise_training}
\title{S-curve dataset with noise dimensions for training}
\format{
A data frame with 75 rows and 8 columns:
\describe{
  \item{ID}{Identification number}
  \item{x1, x2, x3, x4, x5, x6, x7}{High-dimensional coordinates}
}
}
\source{
This dataset is generated for training purposes.
}
\usage{
data(s_curve_noise_training)
}
\description{
The `s_curve_noise_training` dataset contains training data with dimensions x1,
x2, x3, x4, x5, x6, and x7. Each data point is identified by an ID.
}
\examples{
# Load the s_curve_noise_training dataset
data(s_curve_noise_training)

# Display the first few rows of the dataset
head(s_curve_noise_training)

}
\keyword{datasets}
