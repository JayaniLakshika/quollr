#' Calculate the effective number of bins along x-axis
#'
#' This function calculates the effective number of bins along the x-axis of a hexbin plot.
#'
#' @param .data A data frame, data frame extension (e.g., a tibble), or a lazy data frame (e.g., from dbplyr or dtplyr).
#' @param x A literal variable name representing the x-axis.
#' @param cell_area A numeric value that initialise the area of the hexagon.
#'
#' @return A numeric value representing the effective number of bins along x-axis.
#'
#' @import tibble
#' @export
#'
#' @examples
#' nldr_df <- s_curve_noise_umap
#' calculate_effective_x_bins(nldr_df, x = UMAP1, cell_area = 1)
calculate_effective_x_bins <- function(.data, x = UMAP1, cell_area = 1){

  if (any(is.na(.data |> dplyr::pull({{ x }})))) {
    stop("NAs present")
  }

  if (any(is.infinite(.data |> dplyr::pull({{ x }})))) {
    stop("Inf present")
  }

  if ((cell_area <= 0) || (is.infinite(cell_area))) {
    stop("Invalid cell area value")

  }

  ## To compute the diameter of the hexagon
  cell_diameter <- sqrt(2 * cell_area / sqrt(3))

  ## To compute the range along x-axis
  xwidth <- diff(range(.data |>
                         dplyr::pull({{ x }})))

  num_bins <- ceiling(xwidth/cell_diameter)
  num_bins

}

#' Calculate effective shape parameter value
#'
#' This function calculates the effective shape parameter value for a scatter plot based on the ranges of the x-axis and y-axis.
#'
#' @param .data A data frame, data frame extension (e.g., a tibble), or a lazy data frame (e.g., from dbplyr or dtplyr).
#' @param x A literal variable name representing the x-axis.
#' @param y A literal variable name representing the y-axis.
#'
#' @return A numeric value representing the effective shape parameter.
#'
#' @export
#'
#' @examples
#' nldr_df <- s_curve_noise_umap
#' calculate_effective_shape_value(nldr_df, x = UMAP1, y = UMAP2)
calculate_effective_shape_value <- function(.data, x = UMAP1, y = UMAP2){

  if (any(is.na(.data |> dplyr::pull({{ x }}))) || any(is.na(.data |> dplyr::pull({{ y }})))) {
    stop("NAs present")
  }

  if (any(is.infinite(.data |> dplyr::pull({{ x }}))) || any(is.infinite(.data |> dplyr::pull({{ y }})))) {
    stop("Inf present")
  }

  if ((length(.data |> dplyr::pull({{ x }})) == 1) || (length(.data |> dplyr::pull({{ y }})) == 1)) {
    stop("Presence one observation only")

  }

  ## To compute the range along x-axis
  xwidth <- diff(range(.data |> dplyr::pull({{ x }})))
  ## To compute the range along y-axis
  yheight <- diff(range(.data |> dplyr::pull({{ y }})))


  shape <- yheight/xwidth
  shape
}
