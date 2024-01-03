#' Average High-Dimensional Data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param .data The data frame containing the high-dimensional data.
#' @param column_start_text The text that begin the column name of the high-D data
#'
#' @return A data frame with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across everything select starts_with
#'
#' @examples
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' training_data <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_training.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' UMAP_data_with_hb_id <- nldr_df |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' avg_highD_data(df_all)
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

compute_weights <- function(nldr_df, hb_object) {

  ## To get the average of each bin
  bin_val_hexagons <- nldr_df |>
    dplyr::mutate(hb_id = hb_object@cID) |>
    dplyr::select(-ID) |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  names(bin_val_hexagons) <- c("hb_id", "avg_umap1", "avg_umap2")

  ## To calculate distances from average point

  umap_with_avg_all <- dplyr::inner_join(bin_val_hexagons , nldr_df |>
                                           dplyr::mutate(hb_id = hb_object@cID) |>
                                           dplyr::select(-ID), by = c("hb_id" = "hb_id"))


  umap_with_avg_all_split <- umap_with_avg_all |>
    dplyr::group_by(hb_id) |>
    dplyr::group_split()

  vec <- stats::setNames(1:6, c("hb_id", "avg_umap1", "avg_umap2", "UMAP1", "UMAP2", "distance"))
  weight_df <- dplyr::bind_rows(vec)[0, ]

  for(i in 1:length(umap_with_avg_all_split)){

    weighted_mean_df <- umap_with_avg_all_split[[i]] |> ## These are the weights for weighted mean
      cal_2D_dist_umap()

    weight_df <- dplyr::bind_rows(weight_df, weighted_mean_df)

  }

  return(weight_df)

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
#' nldr_df <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_umap.rds"))
#' training_data <- readRDS(paste0(here::here(), "/quollr/data-raw/s_curve_noise_training.rds"))
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' UMAP_data_with_hb_id <- nldr_df |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' df_bin <- avg_highD_data(df_all)
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2D_dist(.data = tr_from_to_df)
#' show_langevitour(df_all, df_bin, df_bin_centroids, benchmark_value = 0.6,
#' distance = distance_df, distance_col = distance)
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
