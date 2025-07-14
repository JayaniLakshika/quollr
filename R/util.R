#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param nldr_obj A list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data.
#' @param b1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the y axis, height and, width of the hexagon.
#'
#' @examples
#' calc_bins_y(nldr_obj = scurve_model_obj$nldr_obj, b1 = 4, q = 0.1)
#'
#' @export
calc_bins_y <- function(nldr_obj, b1 = 4, q = 0.1) {

  ## To check whether b2 greater than 2
  if (b1 < 2) {
    cli::cli_abort("Number of bins along the x-axis at least should be 2.")
  }

  ## To check whether q is between a specific range
  if (!dplyr::between(q, 0.05, 0.2)) {
    cli::cli_abort("The buffer should be within 0.05 and 0.2.")
  }

  ## To compute the range
  lim1 <- nldr_obj$lim1
  lim2 <- nldr_obj$lim2
  r2 <- diff(lim2)/diff(lim1)

  ## To compute the number of bins along the x-axis
  b2 <- ceiling(1 + ((2 * (r2 + q * (1 + r2)) * (b1 - 1))/(sqrt(3) * (1 + 2 * q))))

  ## Validating and compute horizontal spacing
  check_factor <- (1 /(1 + q)) * (((sqrt(3) * (1 + 2 *q) * (b2 - 1))/(2 * (b1 - 1))) - q)

  if (r2 > check_factor) {

    a1 <- (1 + 2 * q)/(b1 - 1)

  } else {

    a1 <- (2 * (r2 + q * (1 + r2)))/(sqrt(3) * (b2 - 1))

  }

  # To compute height of the hexagon
  a2 <- sqrt(3) * a1/2

  return(list(b2 = b2, a1 = a1, a2 = a2))
}
