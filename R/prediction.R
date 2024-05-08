#' Predict 2D embeddings
#'
#' Given a test dataset, the centroid coordinates of hexagonal bins in  2D and high-dimensional space,
#' predict the 2D embeddings for each data point in the test dataset.
#'
#' @param test_data The test dataset containing high-dimensional coordinates and an unique identifier.
#' @param df_bin_centroids Centroid coordinates of hexagonal bins in 2D space.
#' @param df_bin Centroid coordinates of hexagonal bins in high dimensions.
#' @param type_NLDR The type of non-linear dimensionality reduction (NLDR) used.
#'
#' @return A tibble contains predicted 2D embeddings, ID in the test data, and predicted hexagonal IDs.
#' @importFrom dplyr select
#' @importFrom proxy dist
#' @importFrom tibble tibble
#'
#' @examples
#' model <- fit_highd_model(training_data = s_curve_noise_training,
#' emb_df = s_curve_noise_umap_scaled, bin1 = 3, col_start_highd = "x")
#' df_bin_centroids <- model$df_bin_centroids
#' df_bin <- model$df_bin
#' predict_emb(test_data = s_curve_noise_training, df_bin_centroids = df_bin_centroids,
#' df_bin = df_bin, type_NLDR = "UMAP")
#'
#' @export
predict_emb <- function(test_data, df_bin_centroids, df_bin, type_NLDR) {

  test_data_matrix <- test_data |>
    select(-ID) |>
    as.matrix()

  df_bin_matrix <- df_bin |>
    select(-hb_id) |>
    as.matrix()

  ## Compute distances between nldr coordinates and hex bin centroids
  dist_df <- dist(test_data_matrix, df_bin_matrix, method = "Euclidean")

  ## Columns that gives minimum distances
  min_column <- apply(dist_df, 1, which.min)

  pred_hb_id <- df_bin$hb_id[min_column]

  ## Obtain 2D coordinate of the nearest high-D centroid
  match_indices <- match(pred_hb_id, df_bin_centroids$hexID)

  pred_emb1 = df_bin_centroids$c_x[match_indices]
  pred_emb2 = df_bin_centroids$c_y[match_indices]

  pred_obj <- tibble(pred_emb1 = pred_emb1, pred_emb2 = pred_emb2,
                   ID = test_data$ID, pred_hb_id = pred_hb_id)

  ## Rename column names
  names(pred_obj) <- c(paste0("pred_", type_NLDR, "_", 1:2), "ID", "pred_hb_id")

  return(pred_obj)

}


#' Generate evaluation metrics
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param test_data The data set containing high-dimensional data along with an unique identifier.
#' @param prediction_df The data set with 2D embeddings, IDs, and predicted hexagonal IDs.
#' @param df_bin The data set with averaged/weighted high-dimensional data.
#' @param col_start The text that begin the column name of the high-dimensional data.
#'
#' @return A tibble contains Error, MSE and AIC values.
#'
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#'
#' @examples
#' model <- fit_highd_model(training_data = s_curve_noise_training,
#' emb_df = s_curve_noise_umap_scaled, bin1 = 3, col_start_highd = "x")
#' df_bin_centroids <- model$df_bin_centroids
#' df_bin <- model$df_bin
#' pred_df_test <- predict_emb(test_data = s_curve_noise_training,
#' df_bin_centroids = df_bin_centroids, df_bin = df_bin, type_NLDR = "UMAP")
#' glance(test_data = s_curve_noise_training, prediction_df = pred_df_test,
#' df_bin = df_bin, col_start = "x")
#'
#' @export
glance <- function(test_data, prediction_df, df_bin, col_start = "x") {

  ## Rename columns to avoid conflicts
  names(df_bin)[-1] <- paste0("model_high_d_", names(df_bin)[-1])

  ## Map high-D averaged mean coordinates
  prediction_df <- prediction_df |>
    left_join(df_bin, by = c("pred_hb_id" = "hb_id"))

  prediction_df <- prediction_df |>
    left_join(test_data, by = c("ID" = "ID")) ## Map high-D data

  cols <- paste0(col_start, 1:(NCOL(df_bin) - 1))
  high_d_model_cols <- paste0("model_high_d_", col_start, 1:(NCOL(df_bin) - 1))
  error_cols <- paste0("error_square_", col_start, 1:(NCOL(df_bin) - 1))
  abs_error_cols <- paste0("abs_error_", col_start, 1:(NCOL(df_bin) - 1))

  summary_df <- (prediction_df[, cols] - prediction_df[, high_d_model_cols])^2
  names(summary_df) <- error_cols

  row_wise_total_error <- rowSums(summary_df[, error_cols])

  ## To obtain absolute error
  abs_summary_df <- abs(prediction_df[, cols] - prediction_df[, high_d_model_cols])
  names(abs_summary_df) <- abs_error_cols
  error <- sum(rowSums(abs_summary_df[, abs_error_cols]))

  mse <-  mean(row_wise_total_error)

  summary_df <- tibble(Error = error, MSE = mse)

  return(summary_df)

}

#' Augment Data with Predictions and Error Metrics
#'
#' This function augments a dataset with predictions and error metrics obtained
#' from a nonlinear dimension reduction (NLDR) model.
#'
#' @param df_bin_centroids Centroid coordinates of hexagonal bins in 2D space.
#' @param df_bin Centroid coordinates of hexagonal bins in high dimensions.
#' @param training_data Training data used to fit the model.
#' @param newdata Data to be augmented with predictions and error metrics.
#' If NULL, the training data is used (default is NULL).
#' @param type_NLDR The type of non-linear dimensionality reduction (NLDR) used.
#' @param col_start The text that begin the column name of the high-dimensional data.
#'
#' @return A tibble containing the augmented data with predictions,
#' error metrics, and absolute error metrics.
#'
#' @importFrom dplyr left_join select bind_cols
#' @importFrom tidyselect starts_with
#'
#' @examples
#' model <- fit_highd_model(training_data = s_curve_noise_training,
#' emb_df = s_curve_noise_umap_scaled, bin1 = 3, col_start_highd = "x")
#' df_bin_centroids <- model$df_bin_centroids
#' df_bin <- model$df_bin
#' augment(df_bin_centroids = df_bin_centroids, df_bin = df_bin,
#' training_data = s_curve_noise_training, newdata = NULL, type_NLDR = "UMAP",
#' col_start = "x")
#'
#' @export
augment <- function(df_bin_centroids, df_bin, training_data, newdata = NULL,
                    type_NLDR, col_start) {

  if(is.null(newdata)) {
    newdata <- training_data
  }

  ## Rename columns to avoid conflicts
  names(df_bin)[-1] <- paste0("model_high_d_", names(df_bin)[-1])

  ## Map high-D averaged mean coordinates
  prediction_df <- predict_emb(test_data = newdata, df_bin_centroids = df_bin_centroids,
                               df_bin = df_bin, type_NLDR = type_NLDR)

  prediction_df <- prediction_df |>
    left_join(df_bin, by = c("pred_hb_id" = "hb_id"))

  prediction_df <- prediction_df |>
    left_join(newdata, by = c("ID" = "ID")) ## Map high-D data

  prediction_df <- prediction_df |>
    select("ID", starts_with(col_start),
           "pred_hb_id", starts_with("model_high_d_"))

  cols <- paste0(col_start, 1:(NCOL(df_bin) - 1))
  high_d_model_cols <- paste0("model_high_d_", col_start, 1:(NCOL(df_bin) - 1))
  error_cols <- paste0("error_square_", col_start, 1:(NCOL(df_bin) - 1))
  abs_error_cols <- paste0("abs_error_", col_start, 1:(NCOL(df_bin) - 1))

  summary_df <- (prediction_df[, cols] - prediction_df[, high_d_model_cols])^2
  names(summary_df) <- error_cols

  summary_df$row_wise_total_error <- rowSums(summary_df[, error_cols])

  ## To obtain absolute error
  abs_summary_df <- abs(prediction_df[, cols] - prediction_df[, high_d_model_cols])
  names(abs_summary_df) <- abs_error_cols

  abs_summary_df$row_wise_abs_error <- sum(rowSums(abs_summary_df[, abs_error_cols]))

  fit_data <- dplyr::bind_cols(prediction_df, summary_df, abs_summary_df)
  return(fit_data)

}

