% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_hexgrid.R
\name{geom_hexgrid}
\alias{geom_hexgrid}
\title{Create a hexgrid plot}
\usage{
geom_hexgrid(
  mapping = NULL,
  data = NULL,
  stat = "hexgrid",
  position = "identity",
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Aesthetic mappings for the plot.}

\item{data}{The data to be plotted.}

\item{stat}{The statistical transformation to be applied.}

\item{position}{The position adjustment to be applied.}

\item{show.legend}{Whether to show the legend for this layer.}

\item{na.rm}{Whether to remove missing values.}

\item{inherit.aes}{Whether to inherit aesthetics from the plot or the layer.}

\item{...}{Additional arguments to be passed to the `layer` function.}
}
\value{
A `ggplot2` layer object.
}
\description{
Create a hexgrid plot
}
\examples{
df_bin_centroids <- scurve_model_obj$model_2d |> dplyr::filter(n_h > 10)
ggplot2::ggplot() +
geom_hexgrid(data = df_bin_centroids, mapping = ggplot2::aes(x = c_x, y = c_y))

}
