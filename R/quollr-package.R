#' quollr: Visualising How Nonlinear Dimension Reduction Warps Your Data
#'
#' \pkg{quollr} helps you assess nonlinear dimension reduction (NLDR) layouts
#' by constructing a 2-D hexagonal-bin wireframe model and lifting it into
#' high-dimensional space for visual inspection. This makes it possible to
#' determine which NLDR method and hyper-parameter choices produce the most
#' faithful representation of high-dimensional data.
#'
#' The main workflow is:
#' \enumerate{
#'   \item Fit the model with \code{\link{fit_highd_model}}
#'   \item Combine data and model with \code{\link{comb_data_model}}
#'   \item Inspect in high dimensions with \code{\link{show_langevitour}}
#'   \item Evaluate fit with \code{\link{glance}} and \code{\link{augment}}
#' }
#'
#' @examples
#' # Fit the complete model pipeline
#' model <- fit_highd_model(
#'   highd_data = scurve,
#'   nldr_data  = scurve_umap,
#'   b1         = 15,
#'   q          = 0.1,
#'   hd_thresh  = 0
#' )
#'
#' # Combine data and model for visualisation
#' df <- comb_data_model(
#'   highd_data  = scurve,
#'   model_highd = model$model_highd,
#'   model_2d    = model$model_2d
#' )
#'
#' # Evaluate goodness of fit
#' glance(x = model, highd_data = scurve)
#'
#' # Inspect the lifted model in high dimensions (interactive only)
#' \dontrun{
#'   show_langevitour(
#'     point_data = df,
#'     edge_data  = model$trimesh_data
#'   )
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://jayanilakshika.github.io/quollr/}
#'   \item \url{https://arxiv.org/abs/2506.22051}
#' }
#'
#' @aliases quollr-package
#' @name quollr
#' @docType package
#'
#' @useDynLib quollr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom dplyr across arrange between bind_cols bind_rows count distinct
#'   filter first group_by inner_join left_join mutate n nth pull rename
#'   row_number select summarise summarize ungroup
#' @importFrom ggplot2 Geom GeomPoint GeomSegment Stat aes aes_string
#'   element_blank element_rect element_text geom_path geom_point geom_segment
#'   geom_text ggplot layer theme theme_bw theme_linedraw xlab xlim ylab ylim
#' @importFrom grid grobTree
#' @importFrom interp tri.mesh triangles
#' @importFrom langevitour langevitour
#' @importFrom plotly config highlight layout plot_ly style
#' @importFrom proxy dist
#' @importFrom stats quantile setNames
#' @importFrom tibble add_row as_tibble tibble
#' @importFrom tidyselect all_of everything starts_with
"_PACKAGE"
