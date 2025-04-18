% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projection.R
\name{plot_proj}
\alias{plot_proj}
\title{Plot Projected Data with Axes and Circles}
\usage{
plot_proj(
  projected_df,
  model_df,
  axes,
  circle,
  point_param = c(1.5, 0.5, "#000000"),
  line_param = c(0.5, 0.5, "#000000"),
  plot_limits,
  cex = 2,
  position = c(0.92, 0.92),
  axis_text_size = 3,
  is_category = FALSE
)
}
\arguments{
\item{projected_df}{A data frame containing the projected data.}

\item{model_df}{A data frame containing the model reference data.}

\item{axes}{A data frame or list specifying the axes details.}

\item{circle}{A list defining circle parameters.}

\item{point_param}{A vector specifying point size, alpha, and color (default: c(1.5, 0.5, "#000000")).}

\item{line_param}{A vector specifying line width, alpha, and color (default: c(0.5, 0.5, "#000000")).}

\item{plot_limits}{Limits for the plot axes.}

\item{cex}{Scaling factor for point size (default: 2).}

\item{position}{Position of elements within the plot (default: c(0.92, 0.92)).}

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

distance_df <- s_curve_obj$s_curve_umap_model_distance_df
benchmark <- find_lg_benchmark(distance_edges = distance_df,
distance_col = "distance")

df_bin <- s_curve_obj$s_curve_umap_model_obj$df_bin

## Set the maximum difference as the criteria
distance_df_small_edges <- distance_df |>
  dplyr::filter(distance < benchmark) |>
  dplyr::select(-distance)

proj_obj1 <- get_projection(projection = projection_df, proj_scale = 1,
highd_data = s_curve_noise_training, model_highd = df_bin,
tr_from_to_df = distance_df_small_edges,
axis_param = list(limits = 1, axis_scaled = 1, axis_pos_x = -0.72,
axis_pos_y = -0.72,threshold = 0))

projected_df_n <- proj_obj1$projected_df
model_df <- proj_obj1$model_df
axes <- proj_obj1$axes
circle <- proj_obj1$circle

plot_proj(projected_df = projected_df_n, model_df = model_df,
axes = axes, circle = circle, plot_limits = c(-1, 1))
}
