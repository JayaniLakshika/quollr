#' Compute a benchmark value to remove long edges
#'
#' This function finds the benchmark value to remove long edges based on
#' the differences in a distance column.
#'
#' @param distance_edges The data frame containing the distances.
#' @param distance_col The name of the column containing the distances.
#'
#' @return The benchmark value, which is the first largest difference in the distance column.
#'
#' @importFrom dplyr select mutate arrange distinct across pull nth
#' @importFrom stats quantile
#' @importFrom rlang sym
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
#' y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA, col_start = "UMAP")
#' all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
#' counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_vars = c("from", "to", "distance"))
#' find_lg_benchmark(distance_edges = distance_df, distance_col = "distance")
#'
#' @export
find_lg_benchmark <- function(distance_edges, distance_col) {

  if (any(is.na(distance_edges[[rlang::as_string(rlang::sym(distance_col))]]))) {
    stop("NAs present")
  }

  distance_edges <- distance_edges |>
    dplyr::select(!!rlang::sym(distance_col)) |>
    dplyr::mutate(dplyr::across({
      {
        distance_col
      }
    }, \(x) round(x, 3))) |>
    dplyr::arrange(!!rlang::sym(distance_col)) |>  ## Sort the distances
    dplyr::distinct()  ## Find unique distances

  ## Calculate differences between unique distance

  distance_edges <- distance_edges |>
    dplyr::mutate(difference = append(0, apply(distance_edges, 2, diff))) |>
    dplyr::mutate(dplyr::across(difference, ~ round(., 4)))  ## For simplicity

  benchmark_value_vec <- c()

  ## To find the first largest difference (Define a benchmark value
  ## to remove long edges)
  for (i in 1:dim(distance_edges)[1]) {
    if(!is.na(distance_edges$difference[i + 1])){
      if (distance_edges$difference[i] > distance_edges$difference[i + 1]) {
        if (!(is.na(distance_edges$difference[i]))) {
          benchmark_value_vec[i] <- distance_edges$difference[i]
          break
        }
      }
    }
  }

  benchmark_value <- distance_edges[which(distance_edges$difference == benchmark_value_vec[!(is.na(benchmark_value_vec))]),
                                    1] |>  # To get the first value which contain large difference
    dplyr::pull(distance) |>
    dplyr::nth(1)


  if (is.na(benchmark_value)) {
    ## first quartile used as the default
    benchmark_value <- stats::quantile(distance_edges$distance,
                                       probs = c(0,0.25,0.5,0.75,1), names = FALSE)[2]

  }

  benchmark_value


}

#' Compute mean density of hexagonal bins
#'
#' This function calculates the mean density of hexagonal bins based on their neighboring bins.
#'
#' @param df_bin_centroids A data frame containing information about hexagonal bin centroids,
#' including the hexagon ID and the standard normalized counts (\code{std_counts}).
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#'
#' @return A list contains hexagonal IDs and the mean
#' density of each hexagonal bin based on its neighboring bins.
#'
#' @importFrom dplyr filter
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
#' y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA, col_start = "UMAP")
#' all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
#' counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df)
#' compute_mean_density_hex(df_bin_centroids, num_bins_x = num_bins_x)
#'
#' @export
compute_mean_density_hex <- function(df_bin_centroids, num_bins_x = NA) {

  if (is.na(num_bins_x)) {
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
      dplyr::filter((hexID == (hb_id + 1)) |
                      (hexID == (hb_id - 1)) |
                      (hexID == (hb_id + (num_bins_x + 1))) |
                      (hexID == (hb_id + num_bins_x)) |
                      (hexID == (hb_id - (num_bins_x + 1))) |
                      (hexID == (hb_id - num_bins_x)))

    ## The reason to take the mean is to check the density in a considerable amount
    mean_density <- sum(neighbor_df$std_counts)/NROW(neighbor_df)

    mean_density_vec <- append(mean_density_vec, mean_density)

  }

  if (any(is.na(mean_density_vec))) {
    warning("There are hexagonal bins that don't have any neighbouring bins.")
  }

  return(list(hb_id = hexID_vec, mean_density = mean_density_vec))

}


#' Find low-density Hexagons
#'
#' This function identifies hexagons with low density based on the mean density
#' of their neighboring hexagons.
#'
#' @param df_bin_centroids_all The data frame containing all hexagonal bin centroids.
#' @param num_bins_x Number of bins along the x-axis for hexagon binning.
#' @param df_bin_centroids_low The data frame containing identified low-density hexagonal bin centroids.
#'
#' @return A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
#' @importFrom dplyr filter pull
#' @importFrom stats quantile
#'
#' @examples
#' num_bins_list <- calc_bins(data = s_curve_noise_umap_scaled, x = "UMAP1",
#' y = "UMAP2", hex_size = NA, buffer_x = NA, buffer_y = NA)
#' num_bins_x <- num_bins_list$num_x
#' num_bins_y <- num_bins_list$num_y
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA, col_start = "UMAP")
#' all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
#' counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df)
#' df_bin_centroids_low <- df_bin_centroids |>
#' dplyr::filter(std_counts <= 0.43)
#' find_low_dens_hex(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
#' df_bin_centroids_low = df_bin_centroids_low)
#'
#' @export
find_low_dens_hex <- function(df_bin_centroids_all, num_bins_x,
                                      df_bin_centroids_low) {

  if (is.na(num_bins_x)) {
    stop("Number of bins along x-axis is not defined.")
  }

  if (any(is.na(df_bin_centroids_all$std_counts))) {
    stop("NAs present")
  }

  ## To compute mean density of hexagons
  mean_density_list <- compute_mean_density_hex(df_bin_centroids = df_bin_centroids_all,
                                                num_bins_x = num_bins_x)

  mean_density_df <- as.data.frame(do.call(cbind, mean_density_list))

  ## Take first quartile as the benchmark to remove hexagons using mean_density
  benchmark_mean_dens_rm_hex <- stats::quantile(mean_density_df$mean_density,
                                                probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE)[2]

  ## If df_bin_centroids_low is not defined
  if (NROW(df_bin_centroids_low) == 0) {

    first_qtl_conts <- stats::quantile(df_bin_centroids_all$std_counts,
                    probs = c(0,0.25,0.5,0.75,1))[2]

    df_bin_centroids_low <- df_bin_centroids_all |>
      dplyr::filter(std_counts <= first_qtl_conts)
  }

  ## Obtain the hexagonal bins need to remove
  remove_bins <- mean_density_df |>
    dplyr::filter(hb_id %in% df_bin_centroids_low$hexID) |>
    dplyr::filter(mean_density < benchmark_mean_dens_rm_hex) |>
    dplyr::pull(hb_id)

  if (is.null(remove_bins)) {
    message("Don't need to remove low-density hexagonal bins.")

  }

  return(remove_bins)
}


