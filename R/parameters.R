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
#' nldr_df <- s_curve_noise_umap
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2D_dist(tr_from_to_df)
#' find_benchmark_value(distance_df, distance_col = "distance")
#'
#' @export
find_benchmark_value <- function(.data, distance_col) {
  #browser()

  .data <- .data |>
    dplyr::mutate(dplyr::across({
      {
        distance_col
      }
    }, \(x) round(x, 1)))


  sorted_distance_df <- .data |>
    dplyr::arrange({
      {
        distance_col
      }
    })  ## Sort the distances

  # b <- sorted_distance_df %>%
  #   group_by(distance) %>%
  #   summarise(n = n())
  #
  # benchmark_value <- b$distance[which(b$n == median(b$n))[1]]

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


#' Find Low-Density Hexagons
#'
#' This function identifies hexagons with low density based on the mean density of their neighboring hexagons.
#'
#' @param df_bin_centroids The data frame containing hexagonal bin centroids with density information.
#' @param num_bins_x Number of bins along the x-axis for hexagon binning.
#' @param benchmark_rm_hex The benchmark mean density below which hexagons will be marked for removal.
#'
#' @return A vector containing the IDs of hexagons to be removed.
#'
#' @examples
#' nldr_df <- s_curve_noise_umap
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' find_low_density_hexagons(df_bin_centroids, num_bins)
#'
#'
#' @importFrom stats quantile
#' @importFrom utils head
#'
#' @export
find_low_density_hexagons <- function(df_bin_centroids, num_bins_x, benchmark_rm_hex = NA) {

  df_bin_centroids <- df_bin_centroids |>
    dplyr::mutate(ID = row_number())

  # To store mean densities of hexagons
  mean_density_vec <- c()

  for (i in 1:length(df_bin_centroids$hexID)) {

    df_bin_centroids_coordinates_spec_bin <- df_bin_centroids |>
      filter(hexID == df_bin_centroids$hexID[i])

    available_near_check <- df_bin_centroids |>
      dplyr::filter((hexID == (df_bin_centroids$hexID[i] + 1)) | (hexID == (df_bin_centroids$hexID[i] - 1))) |>
      head(1)

    if (NROW(available_near_check) == 0) {

      df_bin_centroids_coordinates_spec_bin_near1 <- df_bin_centroids |>
        filter((hexID == (df_bin_centroids$hexID[i] + (num_bins_x + 1))) | (hexID == (df_bin_centroids$hexID[i] + num_bins_x)) | (hexID == (df_bin_centroids$hexID[i] - (num_bins_x + 1))) | (hexID == (df_bin_centroids$hexID[i] - num_bins_x))) |>
        head(1)

    } else {

      df_bin_centroids_coordinates_spec_bin_near1 <- df_bin_centroids |>
        filter((hexID == (df_bin_centroids$hexID[i] + 1)) | (hexID == (df_bin_centroids$hexID[i] - 1))) |>
        head(1)

    }

    near_df_1 <- dplyr::bind_rows(df_bin_centroids_coordinates_spec_bin, df_bin_centroids_coordinates_spec_bin_near1)

    start <- unlist(near_df_1[1, c("x","y")])
    end <- unlist(near_df_1[2, c("x","y")])
    nearest_dist <- sqrt(sum((start - end)^2)) # Distance to nearest centroid

    df_bin_centroids$distance <- lapply(seq(nrow(df_bin_centroids)), function(x) {
      start <- unlist(df_bin_centroids[(df_bin_centroids_coordinates_spec_bin |> pull(ID)), c("x","y")])
      end <- unlist(df_bin_centroids[x, c("x","y")])
      sqrt(sum((start - end)^2))})

    df_bin_centroids <- df_bin_centroids %>%
      dplyr::select(names(df_bin_centroids), "distance")

    df_bin_centroids$distance <- round(unlist(df_bin_centroids$distance), 7)

    neighbor_df <- df_bin_centroids |>
      filter(distance == round(nearest_dist, 7))

    mean_density <- neighbor_df |>
      pull(std_counts) |>
      sum()/6 ## The reason to take the mean is to check the density in a considerable amount

    mean_density_vec <- append(mean_density_vec, mean_density)

  }

  df_bin_centroids <- df_bin_centroids |>
    dplyr::mutate(mean_density = mean_density_vec)

  remove_bins <- c()
  keep_bins <- c()

  for (i in 1:length(df_bin_centroids$hexID)) {

    df_bin_centroids_coordinates_spec_bin <- df_bin_centroids |>
      filter(hexID == df_bin_centroids$hexID[i])

    bin_ID <- df_bin_centroids_coordinates_spec_bin |>
      pull(hexID)

    if (is.na(benchmark_rm_hex)) {

      benchmark_rm_hex <- stats::quantile(mean_density_vec, probs = c(0,0.25,0.5,0.75,1))[2]

    }

    if(df_bin_centroids_coordinates_spec_bin$mean_density < benchmark_rm_hex){
      remove_bins <- append(remove_bins, bin_ID)
    } else {
      keep_bins <- append(keep_bins, bin_ID)
    }
  }

  return(remove_bins)
}
