#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param bin1 Number of bins along the x axis.
#' @param s1 The x-coordinate of the hexagonal grid starting point.
#' @param s2 The y-coordinate of the hexagonal grid starting point.
#' @param r2 The range of the original second embedding component.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the y axis and width of the hexagon.
#'
#' @examples
#' range_umap2 <- diff(range(s_curve_noise_umap$UMAP2))
#' calc_bins_y(bin1 = 2, s1 = -0.1, s2 = -0.1, r2 = range_umap2)
#'
#' @export
calc_bins_y <- function(bin1 = 2, s1 = -0.1, s2 = -0.1, r2) {

  ## To check whether bin2 greater than 2
  if (bin1 < 2) {
    stop("Number of bins along the x-axis at least should be 2.")
  }

  ## To check whether s1, s2 is between a specific range
  if (!between(s1, -0.1, -0.05) | !between(s1, -0.1, -0.05)) {
    stop("Starting point coordinates should be within -0.1 and -0.05.")
  }

  ## To compute the number of bins along the x-axis
  bin2 <- ceiling(1 + (2 * (r2-s2) * (bin1 - 1))/(sqrt(3) * (1-s1)))

  ## Validating and compute horizontal spacing
  check_factor <- (sqrt(3) * (1 - s1) * (bin2 - 1))/(2 * (bin1 - 1))

  if (r2 > check_factor) {

    a1 <- (1-s1)/(bin1 - 1)

  } else {

    a1 <- (2 * (r2-s2))/(sqrt(3) * (bin2 - 1))

  }

  return(list(bin2 = bin2, a1 = a1))
}
