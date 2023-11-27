#' Compute the Akaike Information Criterion (AIC) for a given model.
#'
#' @param p Number of dimensions of the data set.
#' @param total Total mean squared error (MSE) of the model.
#' @param num_bins Total number of bins used in the model.
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
  mse <- mean(total) / p
  aic <- 2*num_bins*p + num_obs*p*log(mse)
  return(aic)
}


#' Predict Hexagonal IDs
#'
#' This function predicts hexagonal IDs for a test set based on training data and non-linear dimensionality reduction (NLDR) coordinates.
#'
#' @param training_data The training dataset containing high-dimensional data with IDs.
#' @param nldr_df The non-linear dimensionality reduction data frame containing 2D coordinates of the training data.
#' @param nldr_df_test The predicted non-linear dimensionality reductions for the test set.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#'
#' @return A list containing the predicted data, hexagonal bin centroids, and averaged high-dimensional data.
#'
#' @importFrom dplyr bind_cols select mutate
#' @importFrom class knn
#'
#' @examples
#' training_data <- tibble::tibble(
#'   ID = 1:10,
#'   x1 = rnorm(10),
#'   x2 = rnorm(10)
#' )
#' nldr_df <- tibble::tibble(
#'   UMAP1 = rnorm(10),
#'   UMAP2 = rnorm(10)
#' )
#' nldr_df_test <- tibble::tibble(
#'   UMAP1 = rnorm(5),
#'   UMAP2 = rnorm(5)
#' )
#' num_bins <- 5
#' shape_val <- 1
#' predict_hex_id(training_data, nldr_df, nldr_df_test, num_bins, shape_val)
#'
#' @export
predict_hex_id <- function(training_data, nldr_df, nldr_df_test, num_bins, shape_val) {

  ## To extract bin centroids
  hexbin_data_object <-extract_hexbin_centroids(nldr_df, num_bins, shape_val)

  df_bin_centroids <- hexbin_data_object$hexdf_data

  UMAP_data_with_hb_id <- nldr_df |>
    dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)

  ## To generate a data set with high-D and 2D training data
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)

  ## Averaged on high-D
  df_bin <- avg_highD_data(.data = df_all)

  train_hb_df <- df_bin_centroids |>
    dplyr::select(x, y, hexID)

  pred_hb_id <- class::knn(train_hb_df |> dplyr::select(-hexID), nldr_df_test |> dplyr::select(UMAP1, UMAP2), cl = train_hb_df$hexID)

  pred_data <- nldr_df_test |>
    dplyr::mutate(pred_hb_id = as.numeric(as.character(pred_hb_id)))

  return(list(pred_data = pred_data, df_bin_centroids = df_bin_centroids, df_bin = df_bin))

}

#' Generate Evaluation Data Frame
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param data The data set containing high-dimensional data along with IDs.
#' @param prediction_df The data set with 2D embeddings, IDs, and predicted hexagonal IDs.
#' @param df_bin_centroids The data set with coordinates of hexagonal bin centroids.
#' @param df_bin The data set with averaged/weighted high-dimensional data.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#'
#' @return A tibble containing evaluation metrics based on the provided inputs.
#'
#' @importFrom dplyr select inner_join left_join mutate rowSums pick starts_with
#' @importFrom tibble tibble
#'
#' @examples
#' data <- tibble::tibble(
#'   ID = 1:10,
#'   x1 = rnorm(10),
#'   x2 = rnorm(10)
#' )
#' prediction_df <- tibble::tibble(
#'   pred_hb_id = c(1, 2, 1, 3, 2, 3),
#'   UMAP1 = rnorm(6),
#'   UMAP2 = rnorm(6)
#' )
#' df_bin_centroids <- tibble::tibble(
#'   hexID = 1:3,
#'   x = rnorm(3),
#'   y = rnorm(3)
#' )
#' df_bin <- tibble::tibble(
#'   hb_id = c(1, 2, 3),
#'   avg_x1 = rnorm(3),
#'   avg_x2 = rnorm(3)
#' )
#' num_bins <- 5
#' prediction_df <- prediction_df |>
#' dplyr::mutate(ID = seq_along(pred_hb_id))
#' generate_eval_df(data, prediction_df, df_bin_centroids, df_bin, num_bins)
#'
#' @export
generate_eval_df <- function(data, prediction_df, df_bin_centroids, df_bin, num_bins) {

  ## Generate all possible bin centroids in the full grid
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  df_bin_centroids_filtered <- df_bin_centroids |>
    dplyr::select(hexID, x, y)

  ## To map centroid coordinates to predicted hexID
  prediction_df <- dplyr::inner_join(prediction_df, df_bin_centroids_filtered, by = c("pred_hb_id" = "hexID"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  prediction_df <- prediction_df |>
    dplyr::left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D averaged/weighted mean coordinates

  prediction_df <- prediction_df |>
    dplyr::left_join(data, by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    prediction_df[ , paste0("error_square_x", i)] <- (prediction_df[ , paste0("x", i)] - prediction_df[ , paste0("avg_x", i)])^2

  }

  prediction_df <- prediction_df |>
    dplyr::mutate(total = rowSums(dplyr::pick(tidyselect::starts_with("error_square_x"))))

  prediction_df <- prediction_df |>
    dplyr::mutate(
      aic = compute_aic((NCOL(df_bin) - 1), prediction_df$total, NROW(full_centroid_df), NROW(prediction_df)),
      method2 = prediction_df$total * NROW(full_centroid_df)/NROW(prediction_df),
      method3 = prediction_df$total /NROW(full_centroid_df)

    )

  total_error <- sum(prediction_df$aic)
  totol_error_method_2 <- sum(prediction_df$method2)
  totol_error_method_3 <- sum(prediction_df$method3)


  eval_df <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(prediction_df), total_error = total_error, totol_error_method_2 = totol_error_method_2, totol_error_method_3 = totol_error_method_3)

  return(eval_df)

}
