#' Calculate optimal number of bins in a histogram
#'
#' This function calculates the optimal number of bins in a histogram using the IQR method.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the optimal number of bins.
#'
#' @importFrom stats IQR
#' @export
#'
#' @examples
#' x <- 1:100
#' calculate_opt_bin_val_along_axis(x)
calculate_opt_bin_val_along_axis <- function(x){
  h <- 2 * IQR(x) / length(x)^(1/3) # bin width
  return(h)
}


#' Calculate the effective number of bins along x-axis
#'
#' This function calculates the effective number of bins along the x-axis of a scatter plot.
#'
#' @param .data A data frame, data frame extension (e.g., a tibble), or a lazy data frame (e.g., from dbplyr or dtplyr).
#' @param x A literal variable name representing the x-axis.
#' @param y A literal variable name representing the y-axis.
#'
#' @return A numeric value representing the effective number of bins.
#'
#' @import tibble
#' @export
#'
#' @examples
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' calculate_effective_number_of_bins(data, x, y)
calculate_effective_number_of_bins <- function(.data, x, y){

  bw1 <- calculate_opt_bin_val_along_axis(.data |> dplyr::pull({{ x }}))
  bw2 <- calculate_opt_bin_val_along_axis(.data |> dplyr::pull({{ y }}))
  diameter <- sqrt(bw1^2 + bw2^2)

  xbnds <- range(.data |> dplyr::pull({{ x }}))
  num_bins <- round(diff(xbnds)/diameter, 0)  # This should be an integer
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
