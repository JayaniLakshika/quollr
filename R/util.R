#' Calculate the effective number of bins along x-axis and y-axis
#'
#' This function calculates the effective number of bins along the x and y axes
#' of a hexagonal grid.
#'
#' @param bin1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range 0.05-0.1.
#' @param r2 The ratio of the ranges of the original embedding components.
#'
#' @return A list of numeric values that represents the effective number of
#' bins along the y axis and width of the hexagon.
#' @importFrom dplyr between
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' calc_bins_y(bin1 = 2, q = 0.1, r2 = r2)
#'
#' @export
calc_bins_y <- function(bin1 = 2, q = 0.1, r2) {

  ## To check whether bin2 greater than 2
  if (bin1 < 2) {
    stop("Number of bins along the x-axis at least should be 2.")
  }

  ## To check whether q is between a specific range
  if (!between(q, 0.05, 0.1)) {
    stop("The buffer should be within 0.05 and 0.1.")
  }

  ## To check original data range of embedding component 2 is initialized or not
  if (missing(r2)) {
    stop("The range of the original second embedding component is not initialised.")
  }

  ## To initialise starting point coordinates
  s1 <- -q
  s2 <- -q * r2

  ## To compute the number of bins along the x-axis
  bin2 <- ceiling(1 + (2 * (r2-s2) * (bin1 - 1))/(sqrt(3) * (1-s1)))

  ## Validating and compute horizontal spacing
  check_factor <- (sqrt(3) * (bin2 - 1) * (1-s1))/(2 * (bin1 - 1)) + s2

  if (r2 > check_factor) {

    a1 <- (1-s1)/(bin1 - 1)

  } else {

    a1 <- (2 * (r2-s2))/(sqrt(3) * (bin2 - 1))

  }

  return(list(bin2 = bin2, a1 = a1))
}
