#' Scaling the NLDR data
#'
#' This function scales first and second columns.
#'
#' @param nldr_data A tibble that contains embedding components in the first and second columns.
#'
#' @return A list of a tibble contains scaled first and second columns NLDR data,
#' and numeric vectors representing the limits of the original NLDR data.
#'
#' @examples
#' gen_scaled_data(nldr_data = scurve_umap)
#'
#' @export
gen_scaled_data <- function(nldr_data) {

  # Extract columns as matrix for efficiency
  mat <- as.matrix(nldr_data[, 1:2])

  # Compute limits
  min_y1 <- min(mat[, 1])
  max_y1 <- max(mat[, 1])
  min_y2 <- min(mat[, 2])
  max_y2 <- max(mat[, 2])

  lim1 <- c(min_y1, max_y1)
  lim2 <- c(min_y2, max_y2)

  # Scale values
  mat[, 1] <- (mat[, 1] - min_y1) / (max_y1 - min_y1)
  mat[, 2] <- (mat[, 2] - min_y2) / (max_y1 - min_y1)

  # Recombine with original data (if there are other columns)
  nldr_data[, 1:2] <- mat

  # Add ID for scaled nldr data
  scaled_nldr_data <- tibble::as_tibble(mat) |>
    dplyr::mutate(ID = dplyr::row_number())

  return(list(scaled_nldr = scaled_nldr_data, lim1 = lim1, lim2 = lim2))
}

