#' Find Benchmark Value To Remove Long Edges
#'
#' This function finds the benchmark value to remove long edges based on the differences in a distance column.
#'
#' @param .data The data frame containing the distances.
#' @param distance_col The name of the column containing the distances.
#'
#' @return The benchmark value, which is the first largest difference in the distance column.
#'
#' @importFrom dplyr mutate across arrange pull nth bind_cols
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df)
#' find_benchmark_value(.data = distance_df, distance_col = "distance")
#'
#' @export
find_benchmark_value <- function(.data, distance_col) {

  .data <- .data |>
    dplyr::mutate(dplyr::across({
      {
        distance_col
      }
    }, \(x) round(x, 3)))


  sorted_distance_df <- .data |>
    dplyr::arrange({
      {
        distance_col
      }
    })  ## Sort the distances

  unique_dist <- sorted_distance_df |>
    dplyr::pull({
      {
        distance_col
      }
    }) |>
    unique()  ## Get the unique distances

  dist_u <- tibble::tibble(unique_dist = unique_dist)
  dist_u <- dplyr::bind_cols(dist_u, rbind(NA, apply(dist_u, 2, diff)), .name_repair = "unique_quiet")  ## Calculate differences between unique distance
  names(dist_u)[2] <- "difference"

  dist_u <- dist_u |>
    dplyr::mutate(dplyr::across(difference, \(x) round(x, 4)))  ## For simplicity

  dist_u[is.na(dist_u)] <- 0  ## To replace missing values with zero

  benchmark_value_vec <- c()

  ## To find the first largest difference (Define a benchmark value
  ## to remove long edges)
  for (i in 1:dim(dist_u)[1]) {
    if(!is.na(dist_u$difference[i + 1])){
      if (dist_u$difference[i] > dist_u$difference[i + 1]) {
        if (!(is.na(dist_u$difference[i]))) {
          benchmark_value_vec[i] <- dist_u$difference[i]
          break
        }
      }
    }
  }

  benchmark_value_df <- dist_u[which(dist_u$difference == benchmark_value_vec[!(is.na(benchmark_value_vec))]),
                               1]  # To get the first value which contain large difference
  names(benchmark_value_df) <- "unique_dist"
  benchmark_value <- benchmark_value_df |>
    dplyr::pull(unique_dist) |>
    dplyr::nth(1)
  benchmark_value

}

#' Compute Mean Density of Hexagonal Bins
#'
#' This function calculates the mean density of hexagonal bins based on their neighboring bins.
#'
#' @param df_bin_centroids A data frame containing information about hexagonal bin centroids,
#' including the hexagon ID and the standard normalized counts (\code{std_counts}).
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#'
#' @return A data frame with an additional column, \code{mean_density}, representing the mean
#' density of each hexagonal bin based on its neighboring bins.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' compute_mean_density_hex(df_bin_centroids, num_bins_x)
#'
#' @export
compute_mean_density_hex <- function(df_bin_centroids, num_bins_x) {

  # To store mean densities of hexagons
  mean_density_vec <- c()

  for (i in 1:length(df_bin_centroids$hexID)) {

    ## Identify neighbors of a specific hex bin
    neighbor_df <- df_bin_centroids |>
      dplyr::filter((hexID == (df_bin_centroids$hexID[i] + 1)) | (hexID == (df_bin_centroids$hexID[i] - 1)) |
                      (hexID == (df_bin_centroids$hexID[i] + (num_bins_x + 1))) |
                      (hexID == (df_bin_centroids$hexID[i] + num_bins_x)) |
                      (hexID == (df_bin_centroids$hexID[i] - (num_bins_x + 1))) |
                      (hexID == (df_bin_centroids$hexID[i] - num_bins_x)))

    mean_density <- neighbor_df |>
      dplyr::pull(std_counts) |>
      sum()/NROW(neighbor_df) ## The reason to take the mean is to check the density in a considerable amount

    mean_density_vec <- append(mean_density_vec, mean_density)

  }

  df_bin_centroids <- df_bin_centroids |>
    dplyr::mutate(mean_density = mean_density_vec)

  return(df_bin_centroids)

}


#' Find Low-Density Hexagons
#'
#' This function identifies hexagons with low density based on the mean density of their neighboring hexagons.
#'
#' @param df_bin_centroids_all The data frame containing all hexagonal bin centroids.
#' @param num_bins_x Number of bins along the x-axis for hexagon binning.
#' @param df_bin_centroids_low The data frame containing identified low-density hexagonal bin centroids.
#'
#' @return A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' df_bin_centroids_low <- df_bin_centroids |>
#' dplyr::filter(std_counts <= 0.2222222)
#' find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
#' df_bin_centroids_low = df_bin_centroids_low)
#'
#'
#' @importFrom stats quantile
#'
#' @export
find_low_density_hexagons <- function(df_bin_centroids_all, num_bins_x, df_bin_centroids_low) {
  ## To compute mean density of hexagons
  df_bin_centroids <- compute_mean_density_hex(df_bin_centroids_all, num_bins_x)
  mean_density_vec <- df_bin_centroids$mean_density

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(hexID %in% df_bin_centroids_low$hexID)

  ## Take first quartile
  benchmark_mean_dens_rm_hex <- stats::quantile(mean_density_vec, probs = c(0,0.25,0.5,0.75,1))[2]

  remove_bins <- c()

  ## Check only already identified low-density hexagons
  for (i in 1:length(df_bin_centroids_low$hexID)) {

    df_bin_centroids_coordinates_spec_bin <- df_bin_centroids_low |>
      dplyr::filter(hexID == df_bin_centroids_low$hexID[i])

    bin_ID <- df_bin_centroids_coordinates_spec_bin |>
      dplyr::pull(hexID)


    if(df_bin_centroids_coordinates_spec_bin$mean_density < benchmark_mean_dens_rm_hex){
      remove_bins <- append(remove_bins, bin_ID)
    }
  }

  return(remove_bins)
}
