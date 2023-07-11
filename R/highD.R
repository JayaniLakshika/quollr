avg_highD_data <- function(.data, apply_pca = TRUE) {
  df_b <- .data |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(across(everything(), mean))

  ## To change column names to lower case
  names(df_b) <- tolower(names(df_b))

  if (isTRUE(apply_pca)) {
    ## Column names starts with pc
    df_b <- df_b |>
      dplyr::select(tidyselect::starts_with("pc"), hb_id)

  } else {
    ## Column names starts with x
    df_b <- df_b |>
      dplyr::select(tidyselect::starts_with("x"), hb_id)

  }

  return(df_b)
}

find_benchmark_value <- function(.data, distance_col) {

  .data <- .data |>
    dplyr::mutate(dplyr::across({
      {
        distance_col
      }
    }, \(x) round(x, 4)))


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

show_langevitour <- function(df, df_b, df_b_with_center_data, benchmark_value = NA,
                             distance_df, distance_col, min_points_threshold = NA) {

  ### Define type column
  df <- df |>
    dplyr::mutate(type = "data")  ## original dataset

  df_b <- df_b |>
    dplyr::mutate(type = "model")  ## Data with summarized mean

  df_exe <- dplyr::bind_rows(df_b, df)


  if ((is.na(benchmark_value)) && (is.na(min_points_threshold))) {
    tr1 <- triangulate_bin_centroids(df_b_with_center_data)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    langevitour::langevitour(df_exe[1:(length(df_exe) - 2)], lineFrom = tr_from_to_df$from,
                             lineTo = tr_from_to_df$to, group = df_exe$type)
  } else if ((!(is.na(benchmark_value))) && (is.na(min_points_threshold))) {
    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_df |>
      dplyr::filter({
        {
          distance_col
        }
      } < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour::langevitour(df_exe[1:(length(df_exe) - 2)], lineFrom = distance_df_small_edges$from,
                             lineTo = distance_df_small_edges$to, group = df_exe$type)

  } else if ((is.na(benchmark_value)) && (!(is.na(min_points_threshold)))) {
    df_bin_centroids_filterd <- df_bin_centroids |>
      dplyr::filter(Cell_count > min_points_threshold)

    tr1 <- triangulate_bin_centroids(df_bin_centroids_filterd)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    langevitour::langevitour(df_exe[1:(length(df_exe) - 2)], lineFrom = tr_from_to_df$from,
                             lineTo = tr_from_to_df$to, group = df_exe$type)

  } else if ((!(is.na(benchmark_value))) && (!(is.na(min_points_threshold)))) {

    df_bin_centroids_filterd <- df_bin_centroids |>
      dplyr::filter(Cell_count > min_points_threshold)

    tr1 <- triangulate_bin_centroids(df_bin_centroids_filterd)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    distance_d <- cal_2D_dist(.data = tr_from_to_df)
    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_d |>
      dplyr::filter(distance < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour::langevitour(df_exe[1:(length(df_exe) - 2)], lineFrom = distance_df_small_edges$from,
                             lineTo = distance_df_small_edges$to, group = df_exe$type)

  } else {

  }


}
