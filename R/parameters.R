#' Compute a benchmark value to remove long edges
#'
#' This function finds the benchmark value to remove long edges based on
#' the differences in a distance column.
#'
#' @param distance_edges The tibble contains the distances.
#' @param distance_col The name of the column containing the distances.
#'
#' @return The benchmark value, which is the first largest difference in the distance column.
#'
#' @importFrom dplyr select mutate arrange distinct across pull nth
#' @importFrom stats quantile
#' @importFrom rlang sym
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' num_bins_x <- 4
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2)
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df) |>
#' dplyr::filter(drop_empty == FALSE)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_vars = c("from", "to", "distance"))
#' find_lg_benchmark(distance_edges = distance_df, distance_col = "distance")
#'
#' @export
find_lg_benchmark <- function(distance_edges, distance_col) {

  if (any(is.na(distance_edges[[as_string(sym(distance_col))]]))) {
    stop("NAs present")
  }

  distance_edges <- distance_edges |>
    select(!!sym(distance_col)) |>
    mutate(across({
      {
        distance_col
      }
    }, \(x) round(x, 3))) |>
    arrange(!!sym(distance_col)) |>  ## Sort the distances
    distinct()  ## Find unique distances

  ## Calculate differences between unique distance

  distance_edges <- distance_edges |>
    mutate(difference = append(0, apply(distance_edges, 2, diff))) |>
    mutate(across(difference, ~ round(., 4)))  ## For simplicity

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
    pull(distance) |>
    nth(1)


  if (is.na(benchmark_value)) {
    ## first quartile used as the default
    benchmark_value <- quantile(distance_edges$distance,
                                probs = c(0,0.25,0.5,0.75,1), names = FALSE)[2]

  }

  benchmark_value


}

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
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' num_bins_x <- 4
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2)
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df) |>
#' dplyr::filter(drop_empty == FALSE)
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
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' num_bins_x <- 4
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2)
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df) |>
#' dplyr::filter(drop_empty == FALSE)
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


