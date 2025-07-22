#' Compute mean density of hexagonal bins
#'
#' This function calculates the mean density of hexagonal bins based on their neighboring bins.
#'
#' @param model_2d A tibble that contains information about hexagonal bin centroids,
#' including the hexagon ID and the standardised counts (\code{w_h}).
#' @param b1 The number of bins along the x-axis for the hexagonal grid.
#'
#' @return A tibble contains hexagonal IDs and the mean
#' density of each hexagonal bin based on its neighboring bins.
#'
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#'
#' @examples
#' compute_mean_density_hex(model_2d = scurve_model_obj$model_2d, b1 = 5)
#'
#' @export
compute_mean_density_hex <- function(model_2d, b1 = 5) {

  if (missing(b1)) {
    stop("Number of bins along x axis is not defined.")
  }

  hexID_vec <- model_2d$h

  # To store mean densities of hexagons
  mean_density_vec <- c()

  for (hb_id in hexID_vec) {

    ## Identify neighbors of a specific hex bin
    neighbor_df <- model_2d |>
      filter((h == (hb_id + 1)) | (h == (hb_id - 1)) |
                      (h == (hb_id + (b1 + 1))) |
                      (h == (hb_id + b1)) |
                      (h == (hb_id - (b1 + 1))) |
                      (h == (hb_id - b1)))

    ## The reason to take the mean is to check the density in a considerable amount
    mean_density <- sum(neighbor_df$w_h)/NROW(neighbor_df)

    mean_density_vec <- append(mean_density_vec, mean_density)

  }

  if (any(is.na(mean_density_vec))) {
    warning("There are hexagonal bins that don't have any neighbouring bins.")
  }

  mean_df <- tibble(hb_id = hexID_vec, mean_density = mean_density_vec)

  return(mean_df)

}


#' Find low-density Hexagons
#'
#' This function identifies hexagons with low density based on the mean density
#' of their neighboring hexagons.
#'
#' @param model_2d The tibble that contains all hexagonal bin centroids.
#' @param b1 Number of bins along the x-axis for hexagon binning.
#' @param benchmark_mean_dens A numeric value that contains
#'
#' @return A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
#' @importFrom dplyr filter pull
#' @importFrom stats quantile
#'
#' @examples
#' find_low_dens_hex(model_2d = scurve_model_obj$model_2d, b1 = 5,
#' benchmark_mean_dens = 0.05)
#'
#' @export
find_low_dens_hex <- function(model_2d, b1 = 5, benchmark_mean_dens = 0.05) {

  if (is.na(b1)) {
    stop("Number of bins along x-axis is not defined.")
  }

  ## To compute mean density of hexagons
  mean_density_df <- compute_mean_density_hex(model_2d = model_2d,
                                              b1 = b1)

  ## Obtain the hexagonal bins need to remove
  remove_bins <- mean_density_df |>
    filter(mean_density < benchmark_mean_dens) |>
    pull(hb_id)

  if (is.null(remove_bins)) {
    message("Don't need to remove low-density hexagonal bins.")

  }

  return(remove_bins)
}


