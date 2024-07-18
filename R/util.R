#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param bin1 Number of bins along the x axis.
#' @param r2 The ratio of the ranges of the original embedding components.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the y axis and width of the hexagon.
#' @importFrom dplyr between
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' calc_bins_y(bin1 = 4, r2 = r2, q = 0.1)
#'
#' @export
calc_bins_y <- function(bin1 = 4, r2, q = 0.1) {

  ## To check whether bin2 greater than 2
  if (bin1 < 4) {
    stop("Number of bins along the x-axis at least should be 4.")
  }

  ## To check original data range of embedding component 2 is initialized or not
  if (missing(r2)) {
    stop("The range of the original second embedding component is not initialised.")
  }

  ## To check whether q is between a specific range
  if (!between(q, 0.05, 0.2)) {
    stop("The buffer should be within 0.05 and 0.1.")
  }

  ## To compute the number of bins along the x-axis
  bin2 <- ceiling(1 + ((2 * (r2 + q * (1 + r2)) * (bin1 - 1))/(sqrt(3) * (1 + 2 * q))))

  ## Validating and compute horizontal spacing
  check_factor <- (1 /(1 + q)) * (((sqrt(3) * (1 + 2 *q) * (bin2 - 1))/(2 * (bin1 - 1))) - q)

  if (r2 > check_factor) {

    a1 <- (1 + 2 * q)/(bin1 - 1)

  } else {

    a1 <- (2 * (r2 + q * (1 + r2)))/(sqrt(3) * (bin2 - 1))

  }

  return(list(bin2 = bin2, a1 = a1))
}
