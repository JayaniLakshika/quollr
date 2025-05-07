#' Scaling the NLDR data
#'
#' This function scales first and second columns.
#'
#' @param nldr_data A tibble that contains embedding components in the first and second columns.
#'
#' @return A list of a tibble contains scaled first and second columns, and
#' numeric vectors representing the limits of the original data.
#'
#' @examples
#' gen_scaled_data(nldr_data = scurve_umap)
#'
#' @export
gen_scaled_data <- function(nldr_data) {

  ## To compute limits along each embedding component
  limits <- nldr_data |>
    dplyr::summarise(dplyr::across(c(1, 2), list(min = min, max = max)))

  min_x1 <- limits |> dplyr::pull(1)
  max_x1 <- limits |> dplyr::pull(2)
  min_x2 <- limits |> dplyr::pull(3)
  max_x2 <- limits |> dplyr::pull(4)

  lim1 <- c(min_x1, max_x1)
  lim2 <- c(min_x2, max_x2)

  ## To scale the nldr_data
  nldr_data <- nldr_data |>
    dplyr::mutate(dplyr::across(1, ~ (. - min_x1) / (max_x1 - min_x1)), ## scale: 0-1
                  dplyr::across(2, ~ (. - min_x2) / (max_x1 - min_x1))) ## scale: 0-ymax

  return(list(scaled_nldr = nldr_data, lim1 = lim1, lim2 = lim2))

}

