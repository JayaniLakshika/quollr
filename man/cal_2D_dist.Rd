% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_2d.R
\name{cal_2d_dist}
\alias{cal_2d_dist}
\title{Calculate 2D Euclidean distances between vertices}
\usage{
cal_2d_dist(tr_coord_df, start_x, start_y, end_x, end_y, select_vars)
}
\arguments{
\item{tr_coord_df}{A tibble that contains the x and y coordinates of start
and end points.}

\item{start_x}{Column name for the x-coordinate of the starting point.}

\item{start_y}{Column name for the y-coordinate of the starting point.}

\item{end_x}{Column name for the x-coordinate of the ending point.}

\item{end_y}{Column name for the y-coordinate of the ending point.}

\item{select_vars}{A character vector specifying the columns to be
selected in the resulting data frame.}
}
\value{
A tibble with columns for the starting point, ending point,
and calculated distances.
}
\description{
This function calculates the 2D distances between pairs of points in a data frame.
}
\examples{
tr_from_to_df <- s_curve_obj$s_curve_umap_model_tr_from_to_df
cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from", start_y = "y_from",
end_x = "x_to", end_y = "y_to", select_vars = c("from", "to", "distance"))

}
