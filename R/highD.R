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
#' as.data.frame(do.call(cbind, hex_bin_obj$nldr_data_with_hex_id))
#' compute_weights(nldr_df_with_hex_id = UMAP_data_with_hb_id)
#'
#' @importFrom dplyr group_by summarise across
compute_weights <- function(nldr_df_with_hex_id) {

  ## To get the average of each bin
  bin_val_hexagons <- nldr_df_with_hex_id |>
    dplyr::group_by(hb_id) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean))

  new_col <- paste0("avg_", names(nldr_df_with_hex_id)[1:2] |> tolower())

  names(bin_val_hexagons) <- append("hb_id", new_col)

  ## To calculate distances from average point

  nldr_with_avg_all <- dplyr::inner_join(bin_val_hexagons , nldr_df_with_hex_id,
                                         by = c("hb_id" = "hb_id"))


  nldr_with_avg_all_split <- nldr_with_avg_all |>
    dplyr::group_by(hb_id) |>
    dplyr::group_split()

  col_names1 <- append(names(bin_val_hexagons), names(nldr_df_with_hex_id)[1:2])
  col_names <- append(col_names1, "distance")

  vec <- stats::setNames(1:6, col_names)
  weight_df <- dplyr::bind_rows(vec)[0, ]

  for(i in 1:length(nldr_with_avg_all_split)){

    weighted_mean_df <- nldr_with_avg_all_split[[i]] |> ## These are the weights for weighted mean
      cal_2d_dist(start_x = new_col[1], start_y = new_col[2], end_x = names(nldr_df_with_hex_id)[1],
                  end_y = names(nldr_df_with_hex_id)[2], select_col_vec = col_names)

    weight_df <- dplyr::bind_rows(weight_df, weighted_mean_df)

  }

  return(weight_df)

}


#' Compute Weighted Mean for High-Dimensional Data
#'
#' This function computes the weighted mean of the specified columns in the training data
#' based on the distances from the average points in the non-linear dimensionality reduction (NLDR) space.
#'
#' @param training_data A data frame containing the training data with an ID column.
#' @param nldr_df_with_id A data frame containing 2D embeddings with an unique identifier.
#' @param hb_object An object containing information about hexbin IDs.
#' @param column_start_text The starting text of the column names in the training_data
#' that should be considered for the weighted mean. Default is "x".
#'
#' @return A data frame with the computed weighted mean for each specified column.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' hexdf_data <- hexbin_data_object$hexdf_data
#' hb_object <- hexbin_data_object$hb_data
#' weighted_highD_data(training_data = s_curve_noise_training,
#' nldr_df_with_id = s_curve_noise_umap, hb_object = hb_object, column_start_text = "x")
#'
#' @seealso
#' \code{\link{compute_weights}}
#'
#' @importFrom dplyr bind_cols full_join group_by select summarize across
#' @importFrom tidyselect starts_with
#'
#' @export
weighted_highD_data <- function(training_data, nldr_df_with_id, hb_object, column_start_text = "x") {

  nldr_df_with_id <- nldr_df_with_id |> dplyr::mutate(hb_id = hb_object@cID)
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), nldr_df_with_id)

  weight_df <- compute_weights(nldr_df_with_id |> dplyr::select(-c(ID, hb_id)), hb_object)

  joined_col_names <- names(nldr_df_with_id)[1:2]

  weighted_mean_all <- dplyr::inner_join(df_all, weight_df, by = c("hb_id" = "hb_id",
                                                                   stats::setNames(joined_col_names, joined_col_names))) |>
    mutate(distance_trans =  1/ (distance + 0.05))

  weighted_mean_df_list <- list()

  for (j in 1:(NCOL(training_data |> dplyr::select(-ID)))) {

    weighted_mean_df_list[[j]] <- weighted_mean_all |>
      dplyr::select(hb_id, names(training_data |> dplyr::select(-ID))[j], distance_trans) |>
      dplyr::group_by(hb_id) |>
      dplyr::summarise(dplyr::across(names(training_data |> dplyr::select(-ID))[j], ~ weighted.mean(., distance_trans)))

  }

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
#' distance_df <- cal_2d_dist(tr_from_to_df_coord = tr_from_to_df, start_x = "x_from", start_y = "y_from",
#' end_x = "x_to", end_y = "y_to", select_col_vec = c("from", "to", "distance"))
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
