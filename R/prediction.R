#' Predict 2D embeddings
#'
#' Given a test dataset, the centroid coordinates of hexagonal bins in  2D and high-dimensional space,
#' predict the 2D embeddings for each data point in the test dataset.
#'
#' @param highd_data The test dataset containing high-dimensional coordinates and an unique identifier.
#' @param model_2d Centroid coordinates of hexagonal bins in 2D space.
#' @param model_highd Centroid coordinates of hexagonal bins in high dimensions.
#'
#' @return A tibble contains predicted 2D embeddings, ID in the test data, and predicted hexagonal IDs.
#' @importFrom dplyr select
#' @importFrom proxy dist
#' @importFrom tibble tibble
#'
#' @examples
#' predict_emb(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
#' model_2d = scurve_model_obj$model_2d)
#'
#' @export
predict_emb <- function(highd_data, model_2d, model_highd) {

  test_data_matrix <- highd_data |>
    select(-ID) |>
    as.matrix()

  df_bin_matrix <- model_highd |>
    select(-hb_id) |>
    as.matrix()

  ## Compute distances between nldr coordinates and hex bin centroids
  dist_df <- dist(test_data_matrix, df_bin_matrix, method = "Euclidean")

  ## Columns that gives minimum distances
  min_column <- apply(dist_df, 1, which.min)

  pred_hb_id <- model_highd$hb_id[min_column]

  ## Obtain 2D coordinate of the nearest high-D centroid
  match_indices <- match(pred_hb_id, model_2d$hexID)

  pred_emb1 = model_2d$c_x[match_indices]
  pred_emb2 = model_2d$c_y[match_indices]

  pred_obj <- tibble(pred_emb1 = pred_emb1, pred_emb2 = pred_emb2,
                     ID = highd_data$ID, pred_hb_id = pred_hb_id)

  ## Rename column names
  names(pred_obj) <- c(paste0("pred_emb_", 1:2), "ID", "pred_hb_id")

  return(pred_obj)

}


#' Generate evaluation metrics
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param highd_data The dataset containing high-dimensional coordinates and an unique identifier.
#' @param model_2d Centroid coordinates of hexagonal bins in 2D space.
#' @param model_highd Centroid coordinates of hexagonal bins in high dimensions.
#'
#' @return A tibble contains Error, and MSE values.
#'
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#'
#' @examples
#' glance(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
#' model_2d = scurve_model_obj$model_2d)
#'
#' @export
glance <- function(highd_data, model_2d, model_highd) {

  ## Rename columns to avoid conflicts
  names(model_highd)[-1] <- paste0("model_high_d_", names(model_highd)[-1])

  ## Map high-D averaged mean coordinates
  prediction_df <- predict_emb(highd_data = highd_data,
                               model_2d = model_2d,
                               model_highd = model_highd)

  ## Map high-D averaged mean coordinates
  prediction_df <- prediction_df |>
    left_join(model_highd, by = c("pred_hb_id" = "hb_id"))

  prediction_df <- prediction_df |>
    left_join(highd_data, by = c("ID" = "ID")) ## Map high-D data

  cols <- paste0("x", 1:(NCOL(model_highd) - 1))
  high_d_model_cols <- paste0("model_high_d_x", 1:(NCOL(model_highd) - 1))
  error_cols <- paste0("error_square_x", 1:(NCOL(model_highd) - 1))
  abs_error_cols <- paste0("abs_error_x", 1:(NCOL(model_highd) - 1))

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
#' augment(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
#' model_2d = scurve_model_obj$model_2d)
#'
#' @export
augment <- function(highd_data, model_2d, model_highd) {

  ## Rename columns to avoid conflicts
  names(model_highd)[-1] <- paste0("model_high_d_", names(model_highd)[-1])

  ## Map high-D averaged mean coordinates
  prediction_df <- predict_emb(highd_data = highd_data,
                               model_2d = model_2d,
                               model_highd = model_highd)

  prediction_df <- prediction_df |>
    left_join(model_highd, by = c("pred_hb_id" = "hb_id"))

  prediction_df <- prediction_df |>
    left_join(highd_data, by = c("ID" = "ID")) ## Map high-D data

  prediction_df <- prediction_df |>
    select("ID", starts_with("x"),
           "pred_hb_id", starts_with("model_high_d_"))

  cols <- paste0("x", 1:(NCOL(model_highd) - 1))
  high_d_model_cols <- paste0("model_high_d_x", 1:(NCOL(model_highd) - 1))
  error_cols <- paste0("error_square_x", 1:(NCOL(model_highd) - 1))
  abs_error_cols <- paste0("abs_error_x", 1:(NCOL(model_highd) - 1))

  summary_df <- (prediction_df[, cols] - prediction_df[, high_d_model_cols])^2
  names(summary_df) <- error_cols

  summary_df$row_wise_total_error <- rowSums(summary_df[, error_cols])

  ## To obtain absolute error
  abs_summary_df <- abs(prediction_df[, cols] - prediction_df[, high_d_model_cols])
  names(abs_summary_df) <- abs_error_cols

  abs_summary_df$row_wise_abs_error <- rowSums(abs_summary_df[, abs_error_cols])

  fit_data <- dplyr::bind_cols(prediction_df, summary_df, abs_summary_df)
  return(fit_data)

}

