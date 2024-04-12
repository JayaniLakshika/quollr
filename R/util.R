#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param data A tibble or data frame.
#' @param x The name of the column that contains values along the x-axis.
#' @param y The name of the column that contains values along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer
#' circle surrounded by the hexagon.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the x and y axes of a hexagonal grid.
#'
#' @importFrom rlang sym as_string
#' @importFrom dplyr across summarise mutate pull
#'
#' @examples
#' calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3)
#'
#' @export
calc_bins <- function(data, x, y, hex_size = 0.2, buffer_x = 0.346, buffer_y = 0.3){

  ## Obtain values in x and y axes
  x_values <- data[[rlang::as_string(rlang::sym(x))]]
  y_values <- data[[rlang::as_string(rlang::sym(y))]]

  if (anyNA(x_values) | anyNA(y_values)) {
    stop("NAs present")
  }

  if (any(is.infinite(x_values)) | any(is.infinite(y_values))) {
    stop("Inf present")
  }

  if ((hex_size <= 0) || (is.infinite(hex_size))) {
    stop("Invalid hex size value.")

  }

  ## Initialize horizontal and vertical spacing
  hs <- sqrt(3) * hex_size
  vs <- 1.5 * hex_size

  ## Buffer size is exceeds
  if (buffer_x > round(hs, 3)) {
    stop(paste0("Buffer along the x-axis exceeds than ", hs, ".
                  Need to assign a value less than ", hs, "."))

  } else if (buffer_x <= 0) {

    stop(paste0("Buffer along the x-axis is less than or equal to zero."))

  }

  ## Buffer size is exceeds
  if (buffer_y > round(vs, 3)) {
    stop(paste0("Buffer along the y-axis exceeds than ", vs, ".
                  Need to assign a value less than ", vs, "."))

  } else if (buffer_y <= 0) {

    stop(paste0("Buffer along the y-axis is less than or equal to zero."))

  }

  bins_data <- data |>
    summarise(across(c({{ x }}, {{ y }}), ~ max(.) - min(.))) |>
    mutate(num_x = ceiling((get(!!x) + buffer_x) /hs),
           num_y = ceiling((get(!!y) + buffer_y) /vs))

  num_x <- bins_data |>
    pull(num_x)

  num_y <- bins_data |>
    pull(num_y)

  return(list(num_x = num_x, num_y = num_y))
}
