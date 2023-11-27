#' Average High-Dimensional Data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param .data The data frame containing the high-dimensional data.
#' @param column_start_text The text that begin the column name of the high-D data
#'
#' @return A data frame with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across everything mean select starts_with
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1, ~x2, ~x3, ~hb_id,
#'   1, 2, 3, 1,
#'   4, 5, 6, 2,
#'   7, 8, 9, 1
#' )
#' avg_highD_data(df)
#'
#' @export
avg_highD_data <- function(.data, column_start_text = "x") {
  df_b <- .data |>
    dplyr::select(rsample::starts_with(column_start_text), hb_id) |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  return(df_b)
}

cal_2D_dist_umap <- function(.data){

  .data$distance <- lapply(seq(nrow(.data)), function(x) {
    start <- unlist(.data[x, c("avg_umap1","avg_umap2")])
    end <- unlist(.data[x, c("umap1","umap2")])
    sqrt(sum((start - end)^2))})

  distance_df <- .data %>%
    dplyr::select("hb_id", "avg_umap1","avg_umap2", "umap1","umap2", "distance")

  distance_df$distance <- unlist(distance_df$distance)
  return(distance_df)
}



weighted_highD_data <- function(.data, apply_pca = TRUE) {

  weighted_mean_all <- inner_join(.data, weight_df, by = c("hb_id" = "hb_id", "umap1" = "umap1", "umap2" = "umap2")) |>
    mutate(distance_trans =  1/ (distance + 0.05))

  weighted_mean_df_list <- list()

  for (j in 1:(NCOL(weighted_mean_all) - 8)) {

    weighted_mean_df_list[[j]] <- weighted_mean_all |>
      dplyr::select(hb_id, names(weighted_mean_all)[-((length(weighted_mean_all)-7):length(weighted_mean_all))][j], distance_trans) |>
      group_by(hb_id) |>
      summarise(across(names(weighted_mean_all)[-((length(weighted_mean_all)-7):length(weighted_mean_all))][j], ~ weighted.mean(., distance_trans)))

  }

  weighted_mean <- weighted_mean_df_list %>%
    Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2,by="hb_id"), .)

  if (isTRUE(apply_pca)) {
    ## Column names starts with pc
    weighted_mean <- weighted_mean |>
      dplyr::select(tidyselect::starts_with("pc"), hb_id)

  } else {
    ## Column names starts with x
    weighted_mean <- weighted_mean |>
      dplyr::select(tidyselect::starts_with("x"), hb_id)

  }

  return(weighted_mean)
}


#' Find Benchmark Value
#'
#' This function finds the benchmark value to remove long edges based on the differences in a distance column.
#'
#' @param .data The data frame containing the distances.
#' @param distance_col The name of the column containing the distances.
#'
#' @return The benchmark value, which is the first largest difference in the distance column.
#'
#' @importFrom dplyr mutate across arrange pull nth
#' @importFrom tibble tibble bind_cols
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x, ~y, ~distance,
#'   1, 2, 0.5,
#'   4, 5, 0.7,
#'   7, 8, 0.2
#' )
#' find_benchmark_value(df, distance_col = "distance")
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


#' Show LangeviTour Visualization
#'
#' This function generates a LangeviTour visualization based on different conditions and input parameters.
#'
#' @param df The original dataset.
#' @param df_b The summarized mean dataset.
#' @param df_b_with_center_data The dataset with hexbin centroids.
#' @param benchmark_value The benchmark value used to remove long edges (optional).
#' @param distance_df The distance dataframe.
#' @param distance_col The name of the distance column.
#' @param min_points_threshold The minimum number of points threshold for filtering bin centroids (optional).
#'
#' @importFrom dplyr mutate bind_rows filter
#' @importFrom langevitour langevitour
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1, ~x2, ~hb_id,
#'   1, 2, 1,
#'   4, 5, 2,
#'   7, 8, 3
#' )
#' df_b <- tibble::tribble(
#'   ~x1, ~x2, ~hb_id,
#'   2, 3, 1,
#'   5, 6, 2,
#'   8, 9, 3
#' )
#' df_b_with_center_data <- tibble::tribble(
#'   ~x_val_center, ~y_val_center, ~hb_id,
#'   1.5, 2.5, 1,
#'   4.5, 5.5, 2,
#'   7.5, 8.5, 3
#' )
#' distance_df <- tibble::tribble(
#'   ~from, ~to, ~distance,
#'   1, 2, 0.5,
#'   2, 3, 0.7,
#'   1, 3, 0.2
#' )
#' show_langevitour(df, df_b, df_b_with_center_data, benchmark_value = 0.6,
#' distance_df, distance_col = distance)
#'
#' @export
show_langevitour <- function(df, df_b, df_b_with_center_data, benchmark_value = NA,
                             distance_df, distance_col, min_points_threshold = NA) {

  ### Define type column
  df <- df |>
    dplyr::select(tidyselect::starts_with("x")) |>
    dplyr::mutate(type = "data") ## original dataset

  df_b <- df_b |>
    dplyr::filter(hb_id %in% df_b_with_center_data$hexID) |>
    dplyr::select(-hb_id) |>
    dplyr::mutate(type = "model") ## Data with summarized mean

  df_exe <- dplyr::bind_rows(df_b, df)


  if((is.na(benchmark_value)) && (is.na(min_points_threshold))){

    tr1 <- triangulate_bin_centroids(df_b_with_center_data, x, y)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = tr_from_to_df$from , lineTo = tr_from_to_df$to, group = df_exe$type)
  } else if ((!(is.na(benchmark_value))) && (is.na(min_points_threshold))) {
    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_df %>%
      dplyr::filter({{ distance_col }} < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = distance_df_small_edges$from, lineTo = distance_df_small_edges$to, group = df_exe$type)

  } else if ((is.na(benchmark_value)) && (!(is.na(min_points_threshold)))) {
    df_bin_centroids_filterd <- df_bin_centroids %>%
      dplyr::filter(counts > min_points_threshold)

    tr1 <- triangulate_bin_centroids(df_bin_centroids_filterd, x, y)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = tr_from_to_df$from , lineTo = tr_from_to_df$to, group = df_exe$type)

  }  else if ((!(is.na(benchmark_value))) && (!(is.na(min_points_threshold)))) {

    df_bin_centroids_filterd <- df_bin_centroids %>%
      dplyr::filter(Cell_count > min_points_threshold)

    tr1 <- triangulate_bin_centroids(df_bin_centroids_filterd)
    tr_from_to_df <- generate_edge_info(triangular_object = tr1)

    distance_d <- cal_2D_dist(.data = tr_from_to_df)
    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_d %>%
      dplyr::filter(distance < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = distance_df_small_edges$from, lineTo = distance_df_small_edges$to, group = df_exe$type)

  } else {

  }


}
