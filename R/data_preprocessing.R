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
  min_x1 <- min(mat[, 1])
  max_x1 <- max(mat[, 1])
  min_x2 <- min(mat[, 2])
  max_x2 <- max(mat[, 2])

  lim1 <- c(min_x1, max_x1)
  lim2 <- c(min_x2, max_x2)

  # Scale values
  mat[, 1] <- (mat[, 1] - min_x1) / (max_x1 - min_x1)
  mat[, 2] <- (mat[, 2] - min_x2) / (max_x1 - min_x1)

  # Recombine with original data (if there are other columns)
  nldr_data[, 1:2] <- mat

  return(list(scaled_nldr = nldr_data, lim1 = lim1, lim2 = lim2))
}

