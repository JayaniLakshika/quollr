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
  trimesh_data,
  axis_param
)
}
\arguments{
\item{projection}{A matrix or data frame representing the projection.}

\item{proj_scale}{Scaling factor for the projection.}

\item{highd_data}{A data frame or matrix of high-dimensional data.}

\item{model_highd}{A model object or function used for high-dimensional transformation.}

\item{trimesh_data}{A data frame defining transformation from one space to another.}

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

df_bin <- scurve_model_obj$model_highd
edge_data <- scurve_model_obj$trimesh_data

get_projection(projection = projection_df, proj_scale = 1,
highd_data = scurve, model_highd = df_bin,
trimesh_data = edge_data,
axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
axis_pos_y = -0.72,threshold = 0.09))

}
