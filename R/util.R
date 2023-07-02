#' Calculate optimal number of bins in a histogram
#'
#' @param x A numeric vector with one element.
#'
#' @return A numeric value.
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
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param var1 A literal variable name which represents x-axis.
#' @param var2 A literal variable name which represents y-axis.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' calculate_effective_number_of_bins(data, x, y)
calculate_effective_number_of_bins <- function(.data, var1 = var1, var2 = var2){

  bw1 <- calculate_opt_bin_val_along_axis(.data %>%
                                            pull({{ var1 }}))

  bw2 <- calculate_opt_bin_val_along_axis(.data %>%
                                            pull({{ var2 }}))

  diameter <- sqrt(bw1^2 + bw2^2)

  xbnds <- range(.data %>%
                   pull({{ var1 }}))

  num_bins <- round(diff(xbnds)/diameter, 0) ## This should be an integer
  num_bins

}

#' Calculate effective shape parameter value
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param var1 A literal variable name which represents x-axis.
#' @param var2 A literal variable name which represents y-axis.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
#' calculate_effective_shape_value(data, x, y)
calculate_effective_shape_value <- function(.data, var1 = var1, var2 = var2){
  xwidth <- diff(range(data %>%
                         pull({{ var1 }})))
  yheight <- diff(range(data %>%
                          pull({{ var2 }})))

  shape <- yheight/xwidth # Here, yheight is the range of y and xwidth is the renge of x
  shape

}
