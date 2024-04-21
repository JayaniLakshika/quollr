#' Scaling the data
#'
#' This function scales the x and y coordinates.
#'
#' @param data A tibble or data frame.
#' @param x The name of the column that contains values along the x-axis.
#' @param y The name of the column that contains values along the y-axis.
#' @param hr Numeric value representing the ratio of the hexagon size.
#' Default is 2/sqrt(3).
#'
#' @return A tibble contains scaled x and y coordinates.
#'
#' @importFrom dplyr across summarise mutate pull
#'
#' @examples
#' gen_scaled_data(data = s_curve_noise_umap, x = "UMAP1", y = "UMAP2", hr = 2/sqrt(3))
#'
#' @export
gen_scaled_data <- function(data, x, y, hr = 2/sqrt(3)) {

  ## To compute the aspect ratio
  ar <- data |>
    summarise(across(c({{ x }}, {{ y }}), ~ max(.) - min(.))) |>
    mutate(ar = get(!!y)/get(!!x)) |>
    pull(ar)

  scaled_nldr <- data |>
    mutate(across(c({{ x }}, {{ y }}), ~ (. - min(.)) / (max(.) - min(.)))) |>
    mutate({{ y }} := get(!!y) * (ar / hr) * hr)

  return(scaled_nldr)

}

