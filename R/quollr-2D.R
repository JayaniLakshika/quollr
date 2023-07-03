#' Create hexbin object
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param var1 A literal variable name which represents x-axis.
#' @param var2 A literal variable name which represents y-axis.
#' @param num_bins The number of bins partitioning the range of horizontal limits of the binning region in x
#' @param shape_val The shape = yheight/xwidth of the plotting regions.
#'
#' @return an S4 object of class "hexbin".
#' @importFrom dplyr %>%
#' @name %>%
#' @rdname pipe
#' @importFrom dplyr pull
#' @export calculate_effective_number_of_bins
#' @export calculate_effective_shape_value
#'
#' @examples
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' create_hex_bin(data, var1 = "x", var2 = "y")
create_hex_bin <- function(.data, var1, var2, num_bins = NA, shape_val = NA) {
  if (is.na(num_bins)) {
    num_bins <- calculate_effective_number_of_bins(.data, var1 = var1, var2 = var2)
  }
  if (is.na(shape_val)) {
    shape_val <- calculate_effective_shape_value(.data, var1 = var1, var2 = var2)
  }

  hb <- hexbin::hexbin(.data[[var1]], .data[[var2]], xbins = num_bins, IDs = TRUE, shape = shape_val)
  return(hb)
}
