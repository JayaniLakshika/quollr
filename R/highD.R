#' Average High-Dimensional Data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param .data A data frame containing the high-dimensional data and 2D embeddings with hexagonal bin IDs.
#' @param column_start_text The text that begin the column name of the high-dimensional data
#'
#' @return A data frame with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across select
#' @importFrom rsample starts_with
#' @importFrom tidyselect everything
#'
#' @examples
#' training_data <- s_curve_noise_training
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' avg_highD_data(df_all, column_start_text = "x")
#'
#' @export
avg_highD_data <- function(.data, column_start_text = "x") {
  df_b <- .data |>
    dplyr::select(rsample::starts_with(column_start_text), hb_id) |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  return(df_b)
}


#' Compute Weights for Hexagonal Binning
#'
#' This function computes weights for hexagonal binning based on the average values of each bin and the distances from these averages.
#'
#' @param nldr_df_with_hex_id A data frame with 2D embeddings and hexagonal bin IDs.
#'
#' @return A data frame with weights calculated for each hexagonal bin.
#' @importFrom dplyr group_by summarise across inner_join filter
#' @importFrom tidyselect everything
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' UMAP_data_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))
#' compute_weights(nldr_df_with_hex_id = UMAP_data_with_hb_id)
#'
#' @export
compute_weights <- function(nldr_df_with_hex_id) {

  ## To get the 2D embeddings average of each bin
  bin_val_hexagons <- nldr_df_with_hex_id |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  ## Rename columns of averaged 2D embeddings
  names(bin_val_hexagons) <- c("hb_id", paste0("avg_", tolower(names(nldr_df_with_hex_id)[1:2])))

  ## To calculate distances from average point
  nldr_with_avg_all <- dplyr::inner_join(bin_val_hexagons , nldr_df_with_hex_id,
                                         by = c("hb_id" = "hb_id"))

  col_names <- c(names(bin_val_hexagons), names(nldr_df_with_hex_id)[1:2], "distance")

  ## Initialize the vectors to store data
  hexids <- integer(0)
  emb1_vec <- numeric(0)
  emb2_vec <- numeric(0)
  weight_vec <- numeric(0)

  for(hb_id in unique(nldr_with_avg_all$hb_id)){

    ## These are the weights for weighted mean
    weighted_mean_df <- nldr_with_avg_all |>
      dplyr::filter(hb_id == hb_id) |>
      cal_2d_dist(start_x = col_names[2], start_y = col_names[3], end_x = col_names[4],
                  end_y = col_names[5], select_col_vec = col_names)

    hexids <- c(hexids, weighted_mean_df$hb_id)
    emb1_vec <- c(emb1_vec, weighted_mean_df[[rlang::as_string(rlang::sym(col_names[4]))]])
    emb2_vec <- c(emb2_vec, weighted_mean_df[[rlang::as_string(rlang::sym(col_names[5]))]])
    weight_vec <- c(weight_vec, 1/ (weighted_mean_df$distance + 0.05))

  }

  weight_list <- list(hb_id = hexids, emb1_vec = emb1_vec, emb2_vec = emb2_vec,
                      weight_vec = weight_vec)
  names(weight_list) <- c("hb_id", names(nldr_df_with_hex_id)[1:2], "weights")

  return(weight_list)

}


#' Compute Weighted Mean for High-Dimensional Data
#'
#' This function computes the weighted mean of the specified columns in the training data
#' based on the distances from the average points in the non-linear dimensionality reduction (NLDR) space.
#'
#' @param training_data A data frame containing the training data with an ID column.
#' @param nldr_df_with_hex_id A data frame with 2D embeddings and hexagonal bin IDs.
#' @param column_start_text The starting text of the column names in the training_data
#' that should be considered for the weighted mean. Default is "x".
#'
#' @return A data frame with the computed weighted mean for each specified column.
#' nldr_df_with_hex_id = UMAP_data_with_hb_id, column_start_text = "x")
#' @importFrom dplyr bind_cols inner_join group_by select summarize across full_join
#' @importFrom tidyselect starts_with
#' @importFrom stats setNames
#'
#' @examples
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' UMAP_data_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))
#' weighted_highD_data(training_data = s_curve_noise_training,
#' nldr_df_with_hex_id = UMAP_data_with_hb_id, column_start_text = "x")
#'
#' @export
weighted_highD_data <- function(training_data, nldr_df_with_hex_id,
                                column_start_text = "x") {
  ## Remove ID column from training data
  training_data <- training_data |>
    dplyr::select(-ID)

  ## Join training data with 2D embeddings
  df_all <- dplyr::bind_cols(training_data, nldr_df_with_hex_id)

  ## To obtain weights corresponding to 2D embeddings
  weight_df <- as.data.frame(do.call(cbind,
                                     compute_weights(nldr_df_with_hex_id = nldr_df_with_hex_id)))

  ## Initialize the columns that need to use for joining
  joined_col_names <- names(nldr_df_with_hex_id)[1:2]

  ## To join the training data, 2D embeddings and weights
  weighted_mean_all <- dplyr::inner_join(df_all, weight_df,
                                         by = c("hb_id" = "hb_id",
                                                stats::setNames(joined_col_names,
                                                                joined_col_names)))
  ## List to store weighted means
  weighted_mean_df_list <- list()

  for (j in 1:NCOL(training_data)) {

    ## To compute weighted mean across all high-D coordinates
    weighted_mean_df_list[[j]] <- weighted_mean_all |>
      dplyr::select(hb_id, names(training_data)[j], weights) |>
      dplyr::group_by(hb_id) |>
      dplyr::summarise(dplyr::across(names(training_data)[j], ~ weighted.mean(., weights)))

  }

  ## To combine the elements given in the list
  weighted_mean <- Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2,by="hb_id"),
                          weighted_mean_df_list)


  ## Column names start with x
  weighted_mean <- weighted_mean |>
    dplyr::select(hb_id, tidyselect::starts_with(column_start_text))

  return(weighted_mean)
}



#' Show LangeviTour Visualization
#'
#' This function generates a LangeviTour visualization based on different conditions and input parameters.
#'
#' @param df A data frame containing the high-dimensional data.
#' @param df_b A data frame containing the high-dimensional coordinates of bin centroids/ means.
#' @param df_b_with_center_data The dataset with hexbin centroids/ means.
#' @param benchmark_value The benchmark value used to remove long edges (optional).
#' @param distance_df The distance dataframe.
#' @param distance_col The name of the distance column.
#' @param use_default_benchmark_val Logical, indicating whether to use default
#' benchmark value  to remove long edges(default is FALSE).
#' @param column_start_text The text that begin the column name of the high-D data
#'
#'
#' @return A langevitour object with the model and the high-dimensional data.
#'
#' @importFrom dplyr mutate bind_rows filter select
#' @importFrom langevitour langevitour
#'
#' @examples
#' training_data <- s_curve_noise_training
#' num_bins_x <- calculate_effective_x_bins(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", hex_size = NA, buffer_x = NA)
#' num_bins_y <- calculate_effective_y_bins(nldr_df = s_curve_noise_umap_scaled,
#'  y = "UMAP2", hex_size = NA, buffer_y = NA)
#' hex_bin_obj <- generate_hex_binning_info(nldr_df = s_curve_noise_umap_scaled,
#' x = "UMAP1", y = "UMAP2", num_bins_x = num_bins_x,
#' num_bins_y = num_bins_y, x_start = NA, y_start = NA, buffer_x = NA,
#' buffer_y = NA, hex_size = NA)
#' all_centroids_df <- as.data.frame(do.call(cbind, hex_bin_obj$full_grid_hex_centroids))
#' counts_df <- as.data.frame(do.call(cbind, hex_bin_obj$hex_id_with_std_counts))
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df, counts_df = counts_df)
#' UMAP_data_with_hb_id <- as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' df_bin <- avg_highD_data(df_all, column_start_text = "x")
#' tr1_object <- triangulate_bin_centroids(hex_bin_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_col_vec = c("from", "to", "distance"))
#' show_langevitour(df = df_all, df_b = df_bin, df_b_with_center_data = df_bin_centroids,
#' benchmark_value = 0.75, distance = distance_df, distance_col = "distance",
#' use_default_benchmark_val = FALSE, column_start_text = "x")
#'
#' @export
show_langevitour <- function(df, df_b, df_b_with_center_data, benchmark_value = NA,
                             distance_df, distance_col, use_default_benchmark_val = FALSE,
                             column_start_text = "x") {



  ### Define type column
  df <- df |>
    dplyr::select(tidyselect::starts_with(column_start_text)) |>
    dplyr::mutate(type = "data") ## original dataset

  df_b <- df_b |>
    dplyr::filter(hb_id %in% df_b_with_center_data$hexID) |>
    dplyr::mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
  df_b <- df_b[match(df_b_with_center_data$hexID, df_b$hb_id),] |>
    dplyr::select(-hb_id)

  df_exe <- dplyr::bind_rows(df_b, df)


  if(is.na(benchmark_value)){

    if (isFALSE(use_default_benchmark_val)) {

      tr1 <- triangulate_bin_centroids(hex_bin_df = df_b_with_center_data, x = "c_x", y = "c_y")
      tr_from_to_df <- generate_edge_info(triangular_object = tr1)

      langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = tr_from_to_df$from,
                               lineTo = tr_from_to_df$to, group = df_exe$type, pointSize = 3,
                               levelColors = c("#6a3d9a", "#33a02c"))

    } else {

      benchmark_value <- find_benchmark_value(distance_edges = distance_df, distance_col = distance_col)

      ## Set the maximum difference as the criteria
      distance_df_small_edges <- distance_df |>
        dplyr::filter(!!as.name(distance_col) < benchmark_value)
      ## Since erase brushing is considerd.

      langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = distance_df_small_edges$from,
                               lineTo = distance_df_small_edges$to, group = df_exe$type, pointSize = 3,
                               levelColors = c("#6a3d9a", "#33a02c"))

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

    langevitour::langevitour(df_exe[1:(length(df_exe)-1)], lineFrom = distance_df_small_edges$from,
                             lineTo = distance_df_small_edges$to, group = df_exe$type, pointSize = 3,
                             levelColors = c("#6a3d9a", "#33a02c"))

  }


}
