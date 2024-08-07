% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction.R
\name{augment}
\alias{augment}
\title{Augment Data with Predictions and Error Metrics}
\usage{
augment(
  df_bin_centroids,
  df_bin,
  training_data,
  newdata = NULL,
  type_NLDR,
  col_start = "x"
)
}
\arguments{
\item{df_bin_centroids}{Centroid coordinates of hexagonal bins in 2D space.}

\item{df_bin}{Centroid coordinates of hexagonal bins in high dimensions.}

\item{training_data}{Training data used to fit the model.}

\item{newdata}{Data to be augmented with predictions and error metrics.
If NULL, the training data is used (default is NULL).}

\item{type_NLDR}{The type of non-linear dimensionality reduction (NLDR) used.}

\item{col_start}{The text that begin the column name of the high-dimensional data.}
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
r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
model <- fit_highd_model(training_data = s_curve_noise_training,
emb_df = s_curve_noise_umap_scaled, bin1 = 4, r2 = r2, col_start_highd = "x")
df_bin_centroids <- model$df_bin_centroids
df_bin <- model$df_bin
augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin,
training_data = s_curve_noise_training, newdata = NULL, type_NLDR = "UMAP",
col_start = "x")

}
