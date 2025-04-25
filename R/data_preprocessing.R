#' Scaling the NLDR data
#'
#' This function scales first and second columns.
#'
#' @param data A tibble that contains embedding components in the first and second columns.
#'
#' @return A list of a tibble contains scaled first and second columns, and
#' numeric vectors representing the limits of the original data.
#'
#' @importFrom dplyr across summarise mutate pull
#'
#' @examples
#' gen_scaled_data(data = s_curve_noise_umap)
#'
#' @export
gen_scaled_data <- function(data) {

  ## To compute limits along each embedding component
  limits <- data |>
    summarise(across(c(1, 2), list(min = min, max = max)))

  min_x1 <- limits |> pull(1)
  max_x1 <- limits |> pull(2)
  min_x2 <- limits |> pull(3)
  max_x2 <- limits |> pull(4)

  lim1 <- c(min_x1, max_x1)
  lim2 <- c(min_x2, max_x2)

  ## To scale the data
  data <- data |>
    mutate(across(1, ~ (. - min_x1) / (max_x1 - min_x1)), ## scale: 0-1
           across(2, ~ (. - min_x2) / (max_x1 - min_x1))) ## scale: 0-ymax

  return(list(scaled_nldr = data, lim1 = lim1, lim2 = lim2))

}

