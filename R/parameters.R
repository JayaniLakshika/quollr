#' Compute mean density of hexagonal bins
#'
#' This function calculates the mean density of hexagonal bins based on their neighboring bins.
#'
#' @param df_bin_centroids A tibble that contains information about hexagonal bin centroids,
#' including the hexagon ID and the standard normalized counts (\code{std_counts}).
#' @param bin1 The number of bins along the x-axis for the hexagonal grid.
#'
#' @return A tibble contains hexagonal IDs and the mean
#' density of each hexagonal bin based on its neighboring bins.
#'
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- 4
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
#' compute_mean_density_hex(df_bin_centroids, bin1 = num_bins_x)
#'
#' @export
compute_mean_density_hex <- function(df_bin_centroids, bin1) {

  if (missing(bin1)) {
    stop("Number of bins along x axis is not defined.")
  }

  if (any(is.na(df_bin_centroids$std_counts))) {
    stop("NAs present")
  }

  hexID_vec <- df_bin_centroids$hexID

  # To store mean densities of hexagons
  mean_density_vec <- c()

  for (hb_id in hexID_vec) {

    ## Identify neighbors of a specific hex bin
    neighbor_df <- df_bin_centroids |>
      filter((hexID == (hb_id + 1)) | (hexID == (hb_id - 1)) |
                      (hexID == (hb_id + (bin1 + 1))) |
                      (hexID == (hb_id + bin1)) |
                      (hexID == (hb_id - (bin1 + 1))) |
                      (hexID == (hb_id - bin1)))

    ## The reason to take the mean is to check the density in a considerable amount
    mean_density <- sum(neighbor_df$std_counts)/NROW(neighbor_df)

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
#' @param df_bin_centroids_all The tibble that contains all hexagonal bin centroids.
#' @param bin1 Number of bins along the x-axis for hexagon binning.
#' @param df_bin_centroids_low The tibble that contains identified low-density hexagonal bin centroids.
#'
#' @return A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
#' @importFrom dplyr filter pull
#' @importFrom stats quantile
#'
#' @examples
#' num_bins_x <- 4
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
#' df_bin_centroids_low <- df_bin_centroids |>
#' dplyr::filter(std_counts <= 0.43)
#' find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, bin1 = num_bins_x,
#' df_bin_centroids_low = df_bin_centroids_low)
#'
#' @export
find_low_dens_hex <- function(df_bin_centroids_all, bin1, df_bin_centroids_low) {

  if (is.na(bin1)) {
    stop("Number of bins along x-axis is not defined.")
  }

  if (any(is.na(df_bin_centroids_all$std_counts))) {
    stop("NAs present")
  }

  ## To compute mean density of hexagons
  mean_density_df <- compute_mean_density_hex(df_bin_centroids = df_bin_centroids_all,
                                                bin1 = bin1)

  ## Take first quartile as the benchmark to remove hexagons using mean_density
  benchmark_mean_dens_rm_hex <- quantile(mean_density_df$mean_density,
                                                probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE)[2]

  ## If df_bin_centroids_low is not defined
  if (NROW(df_bin_centroids_low) == 0) {

    first_qtl_conts <- quantile(df_bin_centroids_all$std_counts,
                    probs = c(0,0.25,0.5,0.75,1))[2]

    df_bin_centroids_low <- df_bin_centroids_all |>
      dplyr::filter(std_counts <= first_qtl_conts)
  }

  ## Obtain the hexagonal bins need to remove
  remove_bins <- mean_density_df |>
    filter(hb_id %in% df_bin_centroids_low$hexID) |>
    filter(mean_density < benchmark_mean_dens_rm_hex) |>
    pull(hb_id)

  if (is.null(remove_bins)) {
    message("Don't need to remove low-density hexagonal bins.")

  }

  return(remove_bins)
}


