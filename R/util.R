#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param nldr_data A tibble that contains embedding components in the first and second columns.
#' @param bin1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the y axis, height and, width of the hexagon.
#'
#' @examples
#' calc_bins_y(nldr_data = scurve_umap, bin1 = 4, q = 0.1)
#'
#' @export
calc_bins_y <- function(nldr_data, bin1 = 4, q = 0.1) {

  ## To check whether bin2 greater than 2
  if (bin1 < 2) {
    stop("Number of bins along the x-axis at least should be 2.")
  }

  ## To check whether q is between a specific range
  if (!dplyr::between(q, 0.05, 0.2)) {
    stop("The buffer should be within 0.05 and 0.2.")
  }

  ## To pre-process the data
  nldr_obj <- gen_scaled_data(nldr_data = nldr_data)

  ## To compute the range
  lim1 <- nldr_obj$lim1
  lim2 <- nldr_obj$lim2
  r2 <- diff(lim2)/diff(lim1)

  ## To compute the number of bins along the x-axis
  bin2 <- ceiling(1 + ((2 * (r2 + q * (1 + r2)) * (bin1 - 1))/(sqrt(3) * (1 + 2 * q))))

  ## Validating and compute horizontal spacing
  check_factor <- (1 /(1 + q)) * (((sqrt(3) * (1 + 2 *q) * (bin2 - 1))/(2 * (bin1 - 1))) - q)

  if (r2 > check_factor) {

    a1 <- (1 + 2 * q)/(bin1 - 1)

  } else {

    a1 <- (2 * (r2 + q * (1 + r2)))/(sqrt(3) * (bin2 - 1))

  }

  # To compute height of the hexagon
  a2 <- sqrt(3) * a1/2

  return(list(bin2 = bin2, a1 = a1, a2 = a2))
}
