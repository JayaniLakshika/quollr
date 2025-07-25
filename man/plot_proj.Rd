% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projection.R
\name{plot_proj}
\alias{plot_proj}
\title{Plot Projected Data with Axes and Circles}
\usage{
plot_proj(
  proj_obj,
  point_param = c(1, 0.3, "#66B2CC"),
  line_param = c(0.5, 0.5, "#000000"),
  plot_limits,
  axis_text_size = 3,
  is_category = FALSE
)
}
\arguments{
\item{proj_obj}{An object contains a tibble containing the projected data,
a tibble containing the model reference data, a list specifying the axes details,
and a list defining circle parameters.}

\item{point_param}{A vector specifying point size, alpha, and color (default: c(1, 0.3, "#66B2CC")).}

\item{line_param}{A vector specifying line width, alpha, and color (default: c(0.5, 0.5, "#000000")).}

\item{plot_limits}{Limits for the plot axes.}

\item{axis_text_size}{Size of axis text (default: 3).}

\item{is_category}{Logical indicating if the data is categorical (default: FALSE).}
}
\value{
A ggplot object.
}
\description{
Plot Projected Data with Axes and Circles
}
\examples{
projection_df <- cbind(
c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))

df_bin <- scurve_model_obj$model_highd
edge_data <- scurve_model_obj$trimesh_data

proj_obj1 <- get_projection(projection = projection_df, proj_scale = 1,
highd_data = scurve, model_highd = df_bin,
trimesh_data = edge_data,
axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
axis_pos_y = -0.72, threshold = 0.09))

plot_proj(proj_obj = proj_obj1, plot_limits = c(-1, 1))
}
