#' Calculate the effective number of bins along x-axis
#'
#' This function calculates the effective number of bins along the x-axis of a hexagonal grid.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#' @param buffer_x The buffer size along the x-axis.
#'
#' @return A numeric value representing the effective number of bins along x-axis.
#'
#' @examples
#' calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled, x = "UMAP1",
#' hex_size = NA, buffer_x = NA)
#'
#' @export
calculate_effective_x_bins <- function(nldr_df, x, hex_size = NA, buffer_x = NA){

  x_values <- nldr_df[[rlang::as_string(rlang::sym(x))]]

  if (anyNA(x_values)) {
    stop("NAs present")
  }

  if (any(is.infinite(x_values))) {
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

  ## To compute the range along x-axis
  xwidth <- diff(range(x_values))  + buffer_x

  horizontal_spacing <- sqrt(3) * hex_size

  num_bins <- ceiling(xwidth/horizontal_spacing)
  return(num_bins)
}


#' Calculate the effective number of bins along y-axis
#'
#' This function calculates the effective number of bins along the y-axis of a hexagonal grid.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param hex_size A numeric value that initializes the radius of the outer circle surrounding the hexagon.
#' @param buffer_y The buffer size along the y-axis.
#'
#' @return A numeric value representing the effective number of bins along x-axis.
#'
#' @examples
#' calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled, y = "UMAP2",
#' hex_size = NA, buffer_y = NA)
#'
#' @export
calculate_effective_y_bins <- function(nldr_df, y, hex_size = NA, buffer_y = NA){

  y_values <- nldr_df[[rlang::as_string(rlang::sym(y))]]

  if (anyNA(y_values)) {
    stop("NAs present")
  }

  if (any(is.infinite(y_values))) {
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
  ywidth <- diff(range(y_values))  + buffer_y

  vertical_spacing <- 1.5 * hex_size

  num_bins <- ceiling(ywidth/vertical_spacing)
  return(num_bins)

}


