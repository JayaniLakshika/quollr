#' Predict 2D Embeddings
#'
#' Given a test dataset, the centroid coordinates of hexagonal bins in  2D and high-dimensional space,
#' predict the 2D embeddings for each data point in the test dataset.
#'
#' @param test_data The test dataset containing high-dimensional coordinates and an unique identifier.
#' @param df_bin_centroids Centroid coordinates of hexagonal bins in 2D space.
#' @param df_bin Centroid coordinates of hexagonal bins in high dimensions.
#' @param type_NLDR The type of non-linear dimensionality reduction (NLDR) used. Default is "UMAP".
#'
#' @return A list contains predicted 2D embeddings, ID in the test data, and predicted hexagonal IDs.
#' @importFrom dplyr select
#' @importFrom proxy dist
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
#' UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' df_bin <- avg_highD_data(df_all, column_start_text = "x")
#' predict_emb(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
#' df_bin = df_bin, type_NLDR = "UMAP")
#'
#' @export
predict_emb <- function(test_data, df_bin_centroids, df_bin, type_NLDR = "UMAP") {

  test_data_matrix <- test_data |>
    dplyr::select(-ID) |>
    as.matrix()

  df_bin_matrix <- df_bin |>
    dplyr::select(-hb_id) |>
    as.matrix()

  ## Compute distances between nldr coordinates and hex bin centroids
  dist_df <- proxy::dist(test_data_matrix, df_bin_matrix, method = "Euclidean")

  ## Columns that gives minimum distances
  min_column <- apply(dist_df, 1, which.min)

  pred_hb_id <- df_bin$hb_id[min_column]

  ## Obtain 2D coordinate of the nearest high-D centroid
  match_indices <- match(pred_hb_id, df_bin_centroids$hexID)

  pred_emb1 = df_bin_centroids$c_x[match_indices]
  pred_emb2 = df_bin_centroids$c_y[match_indices]

  pred_obj <- list(pred_emb1 = pred_emb1, pred_emb2 = pred_emb2,
                   ID = test_data$ID, pred_hb_id = pred_hb_id)

  ## Rename list elements
  names(pred_obj) <- c(paste0("pred_", type_NLDR, "_", 1:2), "ID", "pred_hb_id")

  return(pred_obj)

}


#' Compute the Akaike Information Criterion (AIC) for a given model.
#'
#' @param p Number of dimensions of the data set.
#' @param mse Mean squared error (MSE) of the model.
#' @param num_bins Total number of bins without empty bins used in the model.
#' @param num_obs Total number of observations in the training or test set.
#'
#' @return The AIC value for the specified model.
#'
#' @examples
#' # Example usage of compute_aic function
#' p <- 5
#' mse <- 1500
#' num_bins <- 10
#' num_obs <- 100
#' aic_value <- compute_aic(p, mse, num_bins, num_obs)
#' cat("AIC Value:", aic_value, "\n")
#'
#' @export
compute_aic <- function(p, mse, num_bins, num_obs) {
  if (is.infinite(p)) {
    stop("Inf present.")
  }

  if (p == 0) {
    stop("No high_D diensions.")
  }

  if (is.na(p)) {
    stop("Should assign number of dimensions in high-D data.")
  }

  if (is.na(num_bins)) {
    stop("Should assign number of non-emty bins.")
  }

  if (is.na(num_obs)) {
    stop("Should assign number of observations in high-D data.")
  }

  if (is.na(mse)) {
    stop("Total error is missing.")
  }

  aic <- 2*num_bins*p + num_obs*p*log(mse)
  return(aic)
}

#' Generate Evaluation Data Frame
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param test_data The data set containing high-dimensional data along with an unique identifier.
#' @param prediction_df The data set with 2D embeddings, IDs, and predicted hexagonal IDs.
#' @param df_bin The data set with averaged/weighted high-dimensional data.
#' @param col_start The text that begin the column name of the high-D data
#'
#' @return A list contains MSE and AIC values.
#'
#' @importFrom dplyr left_join
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
#' UMAP_data_with_hb_id <- hex_bin_obj$nldr_data_with_hex_id
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' df_bin <- avg_highD_data(df_all, column_start_text = "x")
#' pred_emb_list <- predict_emb(test_data = s_curve_noise_training,
#' df_bin_centroids = df_bin_centroids, df_bin = df_bin, type_NLDR = "UMAP")
#' pred_df_test <- as.data.frame(do.call(cbind, pred_emb_list))
#' gen_summary(test_data = s_curve_noise_training, prediction_df = pred_df_test,
#' df_bin = df_bin, col_start = "x")
#'
#' @export
gen_summary <- function(test_data, prediction_df, df_bin, col_start = "x") {

  ## Rename columns to avoid conflicts
  names(df_bin)[-1] <- paste0("model_high_d_", names(df_bin)[-1])

  prediction_df <- prediction_df |>
    dplyr::left_join(df_bin, by = c("pred_hb_id" = "hb_id")) ## Map high-D averaged/weighted mean coordinates

  prediction_df <- prediction_df |>
    dplyr::left_join(test_data, by = c("ID" = "ID")) ## Map high-D data

  cols <- paste0(col_start, 1:(NCOL(df_bin) - 1))
  high_d_model_cols <- paste0("model_high_d_", col_start, 1:(NCOL(df_bin) - 1))
  error_cols <- paste0("error_square_", col_start, 1:(NCOL(df_bin) - 1))

  summary_df <- (prediction_df[, cols] - prediction_df[, high_d_model_cols])^2
  names(summary_df) <- error_cols

  row_wise_total_error <- rowSums(summary_df[, error_cols])

  mse <-  mean(row_wise_total_error)

  aic <-  compute_aic((NCOL(df_bin) - 1), mse,
                      NROW(df_bin), NROW(test_data))

  return(list(mse = mse, aic = aic))

}



