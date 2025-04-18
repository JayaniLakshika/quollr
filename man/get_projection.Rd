% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projection.R
\name{get_projection}
\alias{get_projection}
\title{Compute Projection for High-Dimensional Data}
\usage{
get_projection(
  projection,
  proj_scale,
  highd_data,
  model_highd,
  tr_from_to_df,
  axis_param
)
}
\arguments{
\item{projection}{A matrix or data frame representing the projection.}

\item{proj_scale}{Scaling factor for the projection.}

\item{highd_data}{A data frame or matrix of high-dimensional data.}

\item{model_highd}{A model object or function used for high-dimensional transformation.}

\item{tr_from_to_df}{A data frame defining transformation from one space to another.}

\item{axis_param}{A list of parameters for axis configuration.}
}
\value{
A data frame or matrix with the transformed projection.
}
\description{
Compute Projection for High-Dimensional Data
}
\examples{
projection_df <- cbind(
c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

distance_df <- s_curve_obj$s_curve_umap_model_distance_df
benchmark <- find_lg_benchmark(distance_edges = distance_df,
distance_col = "distance")

df_bin <- s_curve_obj$s_curve_umap_model_obj$df_bin

## Set the maximum difference as the criteria
distance_df_small_edges <- distance_df |>
  dplyr::filter(distance < benchmark) |>
  dplyr::select(-distance)

get_projection(projection = projection_df, proj_scale = 1,
highd_data = s_curve_noise_training, model_highd = df_bin,
tr_from_to_df = distance_df_small_edges,
axis_param = list(limits = 1, axis_scaled = 1, axis_pos_x = -0.72,
axis_pos_y = -0.72,threshold = 0))

}
