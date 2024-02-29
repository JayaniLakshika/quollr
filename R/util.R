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
#' @importFrom rlang sym as_string
#'
#' @examples
#' calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1", y = "UMAP2",
#' hex_size = NA, buffer_x = NA, buffer_y = NA)
#'
#' @export
calc_bins <- function(data, x, y, hex_size = NA, buffer_x = NA, buffer_y = NA){

  ## Obtain values in x and y axes
  x_values <- data[[rlang::as_string(rlang::sym(x))]]
  y_values <- data[[rlang::as_string(rlang::sym(y))]]


  if (anyNA(x_values) | anyNA(y_values)) {
    stop("NAs present")
  }

  if (any(is.infinite(x_values)) | any(is.infinite(y_values))) {
    stop("Inf present")
  }

  if (is.na(hex_size)) {
    hex_size <- 0.2
    message(paste0("Hex size is set to ", hex_size, "."))

  } else {
    if ((hex_size <= 0) || (is.infinite(hex_size))) {
      stop("Invalid hex size value.")

    }
  }

  ## Initialize horizontal and vertical spacing
  hs <- sqrt(3) * hex_size
  vs <- 1.5 * hex_size

  if (is.na(buffer_x)) {
    buffer_x <- sqrt(3) * hex_size * 1.5
    message(paste0("Buffer along the x-axis is set to ", buffer_x, "."))
  } else {

    ## Buffer size is exceeds
    if (buffer_x > (sqrt(3) * hex_size)) {
      stop(paste0("Buffer along the x-axis exceeds than ", sqrt(3) * hex_size, ".
                  Need to assign a value less than ", sqrt(3) * hex_size, "."))

    } else if (buffer_x <= 0) {

      stop(paste0("Buffer along the x-axis is less than or equal to zero."))

    }


  }

  if (is.na(buffer_y)) {
    buffer_y <- 1.5 * hex_size * 1.5
    message(paste0("Buffer along the y-axis is set to ", buffer_y, "."))
  } else {

    ## Buffer size is exceeds
    if (buffer_y > (1.5 * hex_size)) {
      stop(paste0("Buffer along the y-axis exceeds than ", 1.5 * hex_size, ".
                  Need to assign a value less than ", 1.5 * hex_size, "."))

    } else if (buffer_y <= 0) {

      stop(paste0("Buffer along the y-axis is less than or equal to zero."))

    }
  }

  ## To compute the range along x-axis
  xwidth <- diff(range(x_values))  + buffer_x
  num_x <- ceiling(xwidth/hs)

  ## To compute the range along x-axis
  ywidth <- diff(range(y_values))  + buffer_y
  num_y <- ceiling(ywidth/vs)

  return(list(num_x = num_x, num_y = num_y))
}
