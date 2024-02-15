#' Compute the Akaike Information Criterion (AIC) for a given model.
#'
#' @param p Number of dimensions of the data set.
#' @param total Total mean squared error (MSE) of the model.
#' @param num_bins Total number of bins without empty bins used in the model.
#' @param num_obs Total number of observations in the training or test set.
#'
#' @return The AIC value for the specified model.
#' @export
#'
#' @examples
#' # Example usage of compute_aic function
#' p <- 5
#' total <- 1500
#' num_bins <- 10
#' num_obs <- 100
#' aic_value <- compute_aic(p, total, num_bins, num_obs)
#' cat("AIC Value:", aic_value, "\n")
#'
#'
#' @rdname compute_aic
compute_aic <- function(p, total, num_bins, num_obs) {
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

  if (length(total) == 0) {
    stop("Total error is missing.")
  }

  if (any(is.na(total))) {
    stop("Total error vector presence NAs.")
  }

  mse <- mean(total) / p
  aic <- 2*num_bins*p + num_obs*p*log(mse)
  return(aic)
}

#' Generate Evaluation Data Frame
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param data The data set containing high-dimensional data along with an unique identifier.
#' @param prediction_df The data set with 2D embeddings, IDs, and predicted hexagonal IDs.
#' @param df_bin_centroids The data set with coordinates of hexagonal bin centroids.
#' @param df_bin The data set with averaged/weighted high-dimensional data.
#' @param col_start The text that begin the column name of the high-D data
#'
#' @return A tibble containing evaluation metrics based on the provided inputs.
#'
#' @importFrom dplyr select inner_join left_join mutate pick starts_with
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_mean(nldr_df = s_curve_noise_umap, num_bins_x,
#' shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID), UMAP_data_with_hb_id)
#' df_bin <- avg_highD_data(df_all)
#' pred_df_test <- predict_2d_embeddings(test_data = s_curve_noise_training,
#' df_bin_centroids = df_bin_centroids,
#' df_bin = df_bin, type_NLDR = "UMAP")
#' generate_eval_df(data = s_curve_noise, prediction_df = pred_df_test,
#' df_bin_centroids = df_bin_centroids, df_bin = df_bin, col_start = "x")
#'
#' @export
generate_eval_df <- function(data, prediction_df, df_bin_centroids, df_bin, col_start = "x") {

  ## Generate all possible bin centroids in the full grid
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  df_bin_centroids_filtered <- df_bin_centroids |>
    dplyr::select(hexID, x, y)

  ## To map centroid coordinates to predicted hexID
  prediction_df <- dplyr::inner_join(prediction_df, df_bin_centroids_filtered,
                                     by = c("pred_hb_id" = "hexID"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  prediction_df <- prediction_df |>
    dplyr::left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D averaged/weighted mean coordinates

  prediction_df <- prediction_df |>
    dplyr::left_join(data, by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    prediction_df[ , paste0("error_square_", col_start, i)] <- (prediction_df[ , paste0(col_start, i)] - prediction_df[ , paste0("avg_", col_start, i)])^2

  }

  prediction_df <- prediction_df |>
    dplyr::mutate(total = rowSums(dplyr::pick(tidyselect::starts_with(paste0("error_square_", col_start)))))


  #number_of_bins: Total number of bins with empty bins
  eval_df <- tibble::tibble(number_of_bins = NROW(full_centroid_df),
                            number_of_observations = NROW(prediction_df),
                            total_error = compute_aic((NCOL(df_bin) - 1), prediction_df$total,
                                                      NROW(df_bin_centroids), NROW(prediction_df)),
                            total_mse = mean(prediction_df$total))

  return(eval_df)

}

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
#' @return A data frame with predicted 2D embedding for each data point in the test dataset.
#'
#' @examples
#' model <- fit_high_d_model(training_data = s_curve_noise_training,
#' nldr_df_with_id = s_curve_noise_umap)
#' df_bin_centroids <- model$df_bin_centroids
#' df_bin <- model$df_bin
#' predict_2d_embeddings(test_data = s_curve_noise_test, df_bin_centroids = df_bin_centroids,
#' df_bin = df_bin, type_NLDR = "UMAP")
#'
#' @importFrom dplyr mutate_if bind_rows filter row_number arrange
#' @importFrom proxy dist
#' @export
predict_2d_embeddings <- function(test_data, df_bin_centroids, df_bin, type_NLDR = "UMAP") {

  ## Compute distances between nldr coordinates and hex bin centroids
  dist_df <- proxy::dist(as.matrix(test_data |> dplyr::select(-ID)),
                         as.matrix(df_bin |> dplyr::select(-hb_id)), method = "Euclidean")

  ## Columns that gives minimum distances
  min_column <- apply(dist_df, 1, which.min)

  test_data <- test_data |>
    dplyr::mutate(pred_hb_id = df_bin$hb_id[min_column])

  ## Obtain 2D coordinate of the nearest high-D centroid
  match_indices <- match(test_data$pred_hb_id, df_bin_centroids$hexID)
  predict_centroid_coord_2D <- dplyr::bind_cols(test_data, emb_1 = df_bin_centroids$x[match_indices],
                                                emb_2 = df_bin_centroids$y[match_indices])

  predict_centroid_coord_2D <- predict_centroid_coord_2D |>
    dplyr::select(emb_1, emb_2, ID, pred_hb_id)

  ## Rename columns
  names(predict_centroid_coord_2D) <- c(paste0("pred_", type_NLDR, "_", 1:2), "ID", "pred_hb_id")

  return(predict_centroid_coord_2D)

}


