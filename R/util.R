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
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' calculate_effective_x_bins(data, x)
calculate_effective_x_bins <- function(.data, x, cell_area = 1){

  cell_diameter <- sqrt(2 * cell_area / sqrt(3))

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
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' calculate_effective_shape_value(data, x, y)
calculate_effective_shape_value <- function(.data, x, y){
  xwidth <- diff(range(.data |> dplyr::pull({{ x }})))
  yheight <- diff(range(.data |> dplyr::pull({{ y }})))
  shape <- yheight/xwidth  # Here, yheight is the range of y and xwidth is the range of x
  shape
}
