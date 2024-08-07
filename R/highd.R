#' Create a dataframe with averaged high-dimensional data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param data A tibble that contains the high-dimensional data and embedding
#' with hexagonal bin IDs.
#' @param col_start The text that begin the column name of the high-dimensional data
#'
#' @return A tibble with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across select
#' @importFrom rsample starts_with
#' @importFrom tidyselect everything
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' num_bins_x <- 4
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2)
#' umap_data_with_hb_id <- hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training, umap_data_with_hb_id)
#' avg_highd_data(data = df_all, col_start = "x")
#'
#' @export
avg_highd_data <- function(data, col_start = "x") {

  df_b <- data |>
    select(starts_with(col_start), hb_id) |>
    group_by(hb_id) |>
    summarise(across(everything(), mean))

  return(df_b)
}


#' Visualize the model overlaid on high-dimensional data
#'
#' This function generates a LangeviTour visualization based on different
#' conditions and input parameters.
#'
#' @param df A tibble that contains the high-dimensional data.
#' @param df_b A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param df_b_with_center_data The dataset with hexagonal bin centroids.
#' @param benchmark_value The benchmark value used to remove long edges (optional).
#' @param distance_df The tibble with distance.
#' @param distance_col The name of the distance column.
#' @param use_default_benchmark_val Logical, indicating whether to use default
#' benchmark value  to remove long edges (default is FALSE).
#' @param col_start The text that begin the column name of the high-dimensional data.
#'
#'
#' @return A langevitour object with the model and the high-dimensional data.
#'
#' @importFrom dplyr mutate bind_rows filter select
#' @importFrom langevitour langevitour
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
#' umap_data_with_hb_id <- hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
#' umap_data_with_hb_id)
#' df_bin <- avg_highd_data(data = df_all, col_start = "x")
#' show_langevitour(df = df_all, df_b = df_bin, df_b_with_center_data = df_bin_centroids,
#' benchmark_value = 1.16, distance = distance_df, distance_col = "distance",
#' use_default_benchmark_val = FALSE, col_start = "x")
#'
#' @export
show_langevitour <- function(df, df_b, df_b_with_center_data, benchmark_value,
                             distance_df, distance_col, use_default_benchmark_val = FALSE,
                             col_start) {

  ### Define type column
  df <- df |>
    select(starts_with(col_start)) |>
    mutate(type = "data") ## original dataset

  df_b <- df_b |>
    filter(hb_id %in% df_b_with_center_data$hexID) |>
    mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
  df_b <- df_b[match(df_b_with_center_data$hexID, df_b$hb_id),] |>
    select(-hb_id)

  df_exe <- bind_rows(df_b, df)


  if(missing(benchmark_value)){

    if (isFALSE(use_default_benchmark_val)) {

      tr1 <- tri_bin_centroids(hex_df = df_b_with_center_data, x = "c_x", y = "c_y")
      tr_from_to_df <- gen_edges(tri_object = tr1)

      langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = tr_from_to_df$from,
                  lineTo = tr_from_to_df$to, group = df_exe$type,
                  pointSize = append(rep(2, NROW(df_b)), rep(1, NROW(df))),
                  levelColors = c("#000000", "#33a02c"))

    } else {

      benchmark_value <- find_lg_benchmark(distance_edges = distance_df,
                                           distance_col = distance_col)

      ## Set the maximum difference as the criteria
      distance_df_small_edges <- distance_df |>
        filter(!!as.name(distance_col) < benchmark_value)
      ## Since erase brushing is considered.

      langevitour::langevitour(df_exe[1:(length(df_exe)-1)],
                               lineFrom = distance_df_small_edges$from,
                               lineTo = distance_df_small_edges$to,
                               group = df_exe$type,
                               pointSize = append(rep(2, NROW(df_b)), rep(1, NROW(df))),
                               levelColors = c("#000000", "#33a02c"))

    }

  } else {

    ## Check benchmark value is an accepted one
    if (benchmark_value < min(distance_df[[rlang::as_string(rlang::sym(distance_col))]])) {
      stop("Benchmark value to remove long edges is too small.")

    }

    if (benchmark_value > max(distance_df[[rlang::as_string(rlang::sym(distance_col))]])) {
      stop("Benchmark value to remove long edges is too large.")

    }

    if (isTRUE(use_default_benchmark_val)) {
      stop("Need to set `benchmark_value = NA`.")
    }

    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_df |>
      dplyr::filter((!!as.name(distance_col)) < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)],
                             lineFrom = distance_df_small_edges$from,
                             lineTo = distance_df_small_edges$to,
                             group = df_exe$type,
                             pointSize = append(rep(2, NROW(df_b)), rep(1, NROW(df))),
                             levelColors = c("#000000", "#33a02c"))

  }


}
